{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
------------------------------------------------------------------------
-- |
-- Module      : Codec.Archive.Zip
-- Copyright   : John MacFarlane
-- License     : BSD3
--
-- Maintainer  : John MacFarlane < jgm at berkeley dot edu >
-- Stability   : unstable
-- Portability : so far only tested on GHC
--
-- The zip-archive library provides functions for creating, modifying,
-- and extracting files from zip archives.
--
-- Certain simplifying assumptions are made about the zip archives: in
-- particular, there is no support for strong encryption, zip files that span
-- multiple disks, ZIP64, OS-specific file attributes, or compression
-- methods other than Deflate.  However, the library should be able to
-- read the most common zip archives, and the archives it produces should
-- be readable by all standard unzip programs.
--
-- As an example of the use of the library, a standalone zip archiver
-- and extracter, Zip.hs, is provided in the source distribution.
--
-- For more information on the format of zip archives, consult
-- <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>
------------------------------------------------------------------------

module Codec.Archive.Zip
       (

       -- * Data structures
         Archive (..)
       , Entry (..)
       , CompressionMethod (..)
       , EncryptionMethod (..)
       , ZipOption (..)
       , ZipException (..)
       , emptyArchive

       -- * Pure functions for working with zip archives
       , toArchive
       , toArchiveOrFail
       , fromArchive
       , filesInArchive
       , addEntryToArchive
       , deleteEntryFromArchive
       , findEntryByPath
       , fromEntry
       , fromEncryptedEntry
       , isEncryptedEntry
       , toEntry
#ifndef _WINDOWS
       , isEntrySymbolicLink
       , symbolicLinkEntryTarget
       , entryCMode
#endif

       -- * IO functions for working with zip archives
       , readEntry
       , writeEntry
#ifndef _WINDOWS
       , writeSymbolicLinkEntry
#endif
       , addFilesToArchive
       , extractFilesFromArchive

       ) where

import Data.Time.Calendar ( toGregorian, fromGregorian )
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.LocalTime ( TimeZone(..), TimeOfDay(..), timeToTimeOfDay,
                             getTimeZone )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Bits ( shiftL, shiftR, (.&.), (.|.), xor, testBit )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List (nub, find, intercalate)
import Data.Int (Int64)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Printf
import System.FilePath
import System.Directory
       (doesDirectoryExist, getDirectoryContents,
        createDirectoryIfMissing, getModificationTime,)
import Control.Monad ( when, unless, zipWithM_ )
import qualified Control.Exception as E
import System.IO ( stderr, hPutStrLn )
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as M
import Control.Applicative
#ifdef _WINDOWS
import Data.Char (isLetter)
#else
import System.Posix.Files ( setFileTimes, setFileMode, fileMode, getSymbolicLinkStatus, symbolicLinkMode, readSymbolicLink, isSymbolicLink, unionFileModes, createSymbolicLink, removeLink, FileStatus )
import System.Posix.Types ( CMode(..) )
import Data.List (partition)
import Data.Maybe (fromJust)
#endif

-- from bytestring
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

-- text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding.Error as TE

-- from zlib
import qualified Codec.Compression.Zlib.Raw as Zlib
import qualified Codec.Compression.Zlib.Internal as ZlibInt
import System.IO.Error (isAlreadyExistsError)

-- import Debug.Trace

manySig :: Word32 -> Get a -> Get [a]
manySig sig p = do
    sig' <- lookAhead getWord32le
    if sig == sig'
        then do
            r <- p
            rs <- manySig sig p
            return $ r : rs
        else return []


------------------------------------------------------------------------

-- | Structured representation of a zip archive, including directory
-- information and contents (in lazy bytestrings).
data Archive = Archive
                { zEntries                :: [Entry]              -- ^ Files in zip archive
                , zSignature              :: Maybe B.ByteString   -- ^ Digital signature
                , zComment                :: !B.ByteString        -- ^ Comment for whole zip archive
                } deriving (Read, Show)

instance Binary Archive where
  put = putArchive
  get = getArchive

-- | Representation of an archived file, including content and metadata.
data Entry = Entry
               { eRelativePath            :: FilePath            -- ^ Relative path, using '/' as separator
               , eCompressionMethod       :: !CompressionMethod   -- ^ Compression method
               , eEncryptionMethod        :: !EncryptionMethod    -- ^ Encryption method
               , eLastModified            :: !Integer             -- ^ Modification time (seconds since unix epoch, shifted by the local time zone offset: MSDOS timestamps in zip archives are conventionally local time)
               , eCRC32                   :: !Word32              -- ^ CRC32 checksum
               , eCompressedSize          :: !Word32              -- ^ Compressed size in bytes
               , eUncompressedSize        :: !Word32              -- ^ Uncompressed size in bytes
               , eExtraField              :: !B.ByteString        -- ^ Extra field - unused by this library
               , eFileComment             :: !B.ByteString        -- ^ File comment - unused by this library
               , eVersionMadeBy           :: !Word16              -- ^ Version made by field
               , eInternalFileAttributes  :: !Word16              -- ^ Internal file attributes - unused by this library
               , eExternalFileAttributes  :: !Word32              -- ^ External file attributes (system-dependent)
               , eCompressedData          :: !B.ByteString        -- ^ Compressed contents of file
               } deriving (Read, Show, Eq)

-- | Compression methods.
data CompressionMethod = Deflate
                       | NoCompression
                       deriving (Read, Show, Eq)

data EncryptionMethod = NoEncryption             -- ^ Entry is not encrypted
                      | PKWAREEncryption !Word8  -- ^ Entry is encrypted with the traditional PKWARE encryption
                      deriving (Read, Show, Eq)

-- | The way the password should be verified during entry decryption
data PKWAREVerificationType = CheckTimeByte
                            | CheckCRCByte
                            deriving (Read, Show, Eq)

-- | Options for 'addFilesToArchive' and 'extractFilesFromArchive'.
data ZipOption = OptRecursive               -- ^ Recurse into directories when adding files
               | OptVerbose                 -- ^ Print information to stderr
               | OptDestination FilePath    -- ^ Directory in which to extract
               | OptLocation FilePath !Bool -- ^ Where to place file when adding files and whether to append current path
               | OptPreserveSymbolicLinks   -- ^ Preserve symbolic links as such. This option is ignored on Windows. WARNING: symbolic link targets are not validated on extraction, so they may be absolute or point outside of the destination directory; do not use this option when extracting untrusted archives.
               deriving (Read, Show, Eq)

data ZipException =
    CRC32Mismatch FilePath
  | UnsafePath FilePath
  | CannotWriteEncryptedEntry FilePath
  | Zip64NotSupported String  -- ^ Data too large for the original zip format (which this library is limited to); the ZIP64 extension would be required
  deriving (Show, Typeable, Data, Eq)

instance E.Exception ZipException

-- | A zip archive with no contents.
emptyArchive :: Archive
emptyArchive = Archive
                { zEntries                  = []
                , zSignature              = Nothing
                , zComment                = B.empty }

-- | Reads an 'Archive' structure from a raw zip archive (in a lazy bytestring).
toArchive :: B.ByteString -> Archive
toArchive = decode

-- | Like 'toArchive', but returns an 'Either' value instead of raising an
-- error if the archive cannot be decoded.  NOTE:  This function only
-- works properly when the library is compiled against binary >= 0.7.
-- With earlier versions, it will always return a Right value,
-- raising an error if parsing fails.
toArchiveOrFail :: B.ByteString -> Either String Archive
toArchiveOrFail bs = case decodeOrFail bs of
                           Left (_,_,e)  -> Left e
                           Right (_,_,x) -> Right x

-- | Writes an 'Archive' structure to a raw zip archive (in a lazy bytestring).
-- Throws a pure 'Zip64NotSupported' exception if the archive has 65535
-- or more entries or is 4GB or larger, since this would require the
-- (unsupported) ZIP64 extension.
fromArchive :: Archive -> B.ByteString
fromArchive = encode

-- | Returns a list of files in a zip archive.
filesInArchive :: Archive -> [FilePath]
filesInArchive = map eRelativePath . zEntries

-- | Adds an entry to a zip archive, or updates an existing entry.
addEntryToArchive :: Entry -> Archive -> Archive
addEntryToArchive entry archive =
  let archive'   = deleteEntryFromArchive (eRelativePath entry) archive
      oldEntries = zEntries archive'
  in  archive' { zEntries = entry : oldEntries }

-- | Deletes an entry from a zip archive.
deleteEntryFromArchive :: FilePath -> Archive -> Archive
deleteEntryFromArchive path archive =
  archive { zEntries = [e | e <- zEntries archive
                       , not (eRelativePath e `matches` path)] }

-- | Returns Just the zip entry with the specified path, or Nothing.
findEntryByPath :: FilePath -> Archive -> Maybe Entry
findEntryByPath path archive =
  find (\e -> path `matches` eRelativePath e) (zEntries archive)

-- | Returns uncompressed contents of zip entry.
fromEntry :: Entry -> B.ByteString
fromEntry entry =
  decompressData (eCompressionMethod entry) (eCompressedData entry)

-- | Returns decrypted and uncompressed contents of zip entry.
fromEncryptedEntry :: String -> Entry -> Maybe B.ByteString
fromEncryptedEntry password entry =
  decompressData (eCompressionMethod entry) <$> decryptData password (eEncryptionMethod entry) (eCompressedData entry)

-- | Check if an 'Entry' is encrypted
isEncryptedEntry :: Entry -> Bool
isEncryptedEntry entry =
  case eEncryptionMethod entry of
    (PKWAREEncryption _) -> True
    _ -> False

-- | Create an 'Entry' with specified file path, modification time, and contents.
-- Throws a pure 'Zip64NotSupported' exception if the contents are too
-- large to be represented without the (unsupported) ZIP64 extension.
toEntry :: FilePath         -- ^ File path for entry
        -> Integer          -- ^ Modification time for entry (seconds since unix epoch)
        -> B.ByteString     -- ^ Contents of entry
        -> Entry
toEntry path modtime contents =
  let uncompressedSize = B.length contents
      compressedData = compressData Deflate contents
      compressedSize = B.length compressedData
      -- only use compression if it helps!
      (compressionMethod, finalData, finalSize) =
        if uncompressedSize <= compressedSize
           then (NoCompression, contents, uncompressedSize)
           else (Deflate, compressedData, compressedSize)
      crc32 = CRC32.crc32 contents
  in  if uncompressedSize >= 0xFFFFFFFF
         then E.throw $ Zip64NotSupported $
                path ++ ": entry of 4GB or more requires ZIP64"
         else
      Entry { eRelativePath            = normalizePath path
            , eCompressionMethod       = compressionMethod
            , eEncryptionMethod        = NoEncryption
            , eLastModified            = modtime
            , eCRC32                   = crc32
            , eCompressedSize          = fromIntegral finalSize
            , eUncompressedSize        = fromIntegral uncompressedSize
            , eExtraField              = B.empty
            , eFileComment             = B.empty
            , eVersionMadeBy           = 0  -- FAT
            , eInternalFileAttributes  = 0  -- potentially non-text
            , eExternalFileAttributes  = 0  -- appropriate if from stdin
            , eCompressedData          = finalData
            }

-- | Generates a 'Entry' from a file or directory.
readEntry :: [ZipOption] -> FilePath -> IO Entry
readEntry opts path = do
  isDir <- doesDirectoryExist path
#ifdef _WINDOWS
  let isSymLink = False
#else
  fs <- getSymbolicLinkStatus path
  let isSymLink = isSymbolicLink fs
#endif
 -- make sure directories end in / and deal with the OptLocation option
  let path' = let p = path ++ (case reverse path of
                                    ('/':_) -> ""
                                    _ | isDir && not isSymLink -> "/"
                                    _ | isDir && isSymLink -> ""
                                      | otherwise -> "") in
              (case [(l,a) | OptLocation l a <- opts] of
                    ((l,a):_) -> if a then l </> p else l </> takeFileName p
                    _         -> p)
  contents <-
#ifndef _WINDOWS
              if isSymLink
                 then do
                   linkTarget <- readSymbolicLink path
                   return $ C.pack linkTarget
                 else
#endif
                   if isDir
                      then
                        return B.empty
                      else
                        B.fromStrict <$> S.readFile path
  modTime <- getModificationTime path
  tzone <- getTimeZone modTime
  let modEpochTime = -- UNIX time computed relative to LOCAL time zone! (#67)
        floor (utcTimeToPOSIXSeconds modTime) +
          fromIntegral (timeZoneMinutes tzone * 60)
  let entry = toEntry path' modEpochTime contents

  entryE <-
#ifdef _WINDOWS
        return $ entry { eVersionMadeBy = 0x0000 } -- FAT/VFAT/VFAT32 file attributes
#else
        do
           let fm = if isSymLink
                      then unionFileModes symbolicLinkMode (fileMode fs)
                      else fileMode fs

           let modes = fromIntegral $ shiftL (toInteger fm) 16
           return $ entry { eExternalFileAttributes = modes,
                            eVersionMadeBy = 0x0300 } -- UNIX file attributes
#endif

  when (OptVerbose `elem` opts) $ do
    let compmethod = case eCompressionMethod entryE of
                     Deflate       -> ("deflated" :: String)
                     NoCompression -> "stored"
    hPutStrLn stderr $
      printf "  adding: %s (%s %.f%%)" (eRelativePath entryE)
      compmethod (100 - (100 * compressionRatio entryE))
  return entryE

-- check path: reject absolute paths and drive-qualified paths, and
-- resolve .. and . components, raising UnsafePath exception if this
-- takes you outside of the root.
checkPath :: FilePath -> IO ()
checkPath fp
  | isAbsolute fp || hasDrive fp = E.throwIO (UnsafePath fp)
  | otherwise =
      maybe (E.throwIO (UnsafePath fp)) (\_ -> return ())
        (resolve . splitDirectories $ fp)
  where
    resolve =
      fmap reverse . foldl go (return [])
      where
      go acc x = do
        xs <- acc
        case x of
          "."  -> return xs
          ".." -> case xs of
                    []     -> fail "outside of root path"
                    (_:ys) -> return ys
          _    -> return (x:xs)

-- | Writes contents of an 'Entry' to a file.  Throws a
-- 'CRC32Mismatch' exception if the CRC32 checksum for the entry
-- does not match the uncompressed data.
writeEntry :: [ZipOption] -> Entry -> IO ()
writeEntry opts entry = do
  when (isEncryptedEntry entry) $
    E.throwIO $ CannotWriteEncryptedEntry (eRelativePath entry)
  let relpath = eRelativePath entry
  checkPath relpath
  path <- case [d | OptDestination d <- opts] of
             (x:_) -> return (x </> relpath)
             []    -> return relpath
  -- create directories if needed
  let dir = takeDirectory path
  exists <- doesDirectoryExist dir
  unless exists $ do
    createDirectoryIfMissing True dir
    when (OptVerbose `elem` opts) $
      hPutStrLn stderr $ "  creating: " ++ dir
  if not (null path) && last path == '/' -- path is a directory
     then return ()
     else do
       when (OptVerbose `elem` opts) $
         hPutStrLn stderr $ case eCompressionMethod entry of
                                 Deflate       -> " inflating: " ++ path
                                 NoCompression -> "extracting: " ++ path
       let uncompressedData = fromEntry entry
       if eCRC32 entry == CRC32.crc32 uncompressedData
          then B.writeFile path uncompressedData
          else E.throwIO $ CRC32Mismatch path
#ifndef _WINDOWS
       let modes = fromIntegral $ shiftR (eExternalFileAttributes entry) 16
       when (eVersionMadeBy entry .&. 0xFF00 == 0x0300 &&
         modes /= 0) $ setFileMode path modes
#endif
  -- Note that last modified times are supported only for POSIX, not for
  -- Windows.
  setFileTimeStamp path (eLastModified entry)

#ifndef _WINDOWS
-- | Write an 'Entry' representing a symbolic link to a file.
-- If the 'Entry' does not represent a symbolic link or
-- the options do not contain 'OptPreserveSymbolicLinks`, this
-- function behaves like `writeEntry`.
--
-- Note that the symbolic link target is written as is; it may be
-- absolute or point outside of the extraction directory.  Do not
-- extract untrusted archives with 'OptPreserveSymbolicLinks'.
writeSymbolicLinkEntry :: [ZipOption] -> Entry -> IO ()
writeSymbolicLinkEntry opts entry =
  if OptPreserveSymbolicLinks `notElem` opts
     then writeEntry opts entry
     else do
        if isEntrySymbolicLink entry
           then do
             let relpath = eRelativePath entry
             checkPath relpath
             let prefixPath = case [d | OptDestination d <- opts] of
                                   (x:_) -> x
                                   _     -> ""
             checkSymbolicLinkAncestry prefixPath relpath
             let targetPath = fromJust . symbolicLinkEntryTarget $ entry
             let symlinkPath = prefixPath </> relpath
             when (OptVerbose `elem` opts) $ do
               hPutStrLn stderr $ "linking " ++ symlinkPath ++ " to " ++ targetPath
             forceSymLink targetPath symlinkPath
           else writeEntry opts entry

-- Guard against symlink chaining on extraction: raise 'UnsafePath' if
-- any directory component of relpath (relative to prefix) is itself a
-- symbolic link.  Otherwise a crafted archive containing a symbolic
-- link entry @a -> /somewhere@ followed by an entry @a/b@ could create
-- a symbolic link outside of the destination directory.
checkSymbolicLinkAncestry :: FilePath -> FilePath -> IO ()
checkSymbolicLinkAncestry prefix relpath =
  mapM_ check $ scanl1 (</>) ancestors
  where
    ancestors = case splitDirectories relpath of
                     [] -> []
                     cs -> init cs
    check dir = do
      res <- E.try (getSymbolicLinkStatus (prefix </> dir))
                :: IO (Either E.IOException FileStatus)
      case res of
        Right st | isSymbolicLink st -> E.throwIO (UnsafePath relpath)
        _                            -> return ()


-- | Writes a symbolic link, but removes any conflicting files and retries if necessary.
forceSymLink :: FilePath -> FilePath -> IO ()
forceSymLink target linkName =
    createSymbolicLink target linkName `E.catch`
      (\e -> if isAlreadyExistsError e
             then removeLink linkName >> createSymbolicLink target linkName
             else ioError e)

-- | Get the target of a 'Entry' representing a symbolic link. This might fail
-- if the 'Entry' does not represent a symbolic link
symbolicLinkEntryTarget :: Entry -> Maybe FilePath
symbolicLinkEntryTarget entry | isEntrySymbolicLink entry = Just . C.unpack $ fromEntry entry
                              | otherwise = Nothing

-- | Check if an 'Entry' represents a symbolic link
isEntrySymbolicLink :: Entry -> Bool
isEntrySymbolicLink entry = entryCMode entry .&. symbolicLinkMode == symbolicLinkMode

-- | Get the 'eExternalFileAttributes' of an 'Entry' as a 'CMode' a.k.a. 'FileMode'
entryCMode :: Entry -> CMode
entryCMode entry = CMode (fromIntegral $ shiftR (eExternalFileAttributes entry) 16)
#endif

-- | Add the specified files to an 'Archive'.  If 'OptRecursive' is specified,
-- recursively add files contained in directories. if 'OptPreserveSymbolicLinks'
-- is specified, don't recurse into it. If 'OptVerbose' is specified,
-- print messages to stderr.
addFilesToArchive :: [ZipOption] -> Archive -> [FilePath] -> IO Archive
addFilesToArchive opts archive files = do
  filesAndChildren <- if OptRecursive `elem` opts
#ifdef _WINDOWS
                         then mapM getDirectoryContentsRecursive files >>= return . nub . concat
#else
                         then nub . concat <$> mapM (getDirectoryContentsRecursive' opts) files
#endif
                         else return files
  entries <- mapM (readEntry opts) filesAndChildren
  return $ foldr addEntryToArchive archive entries

-- | Extract all files from an 'Archive', creating directories
-- as needed.  If 'OptVerbose' is specified, print messages to stderr.
-- Note that the last-modified time is set correctly only in POSIX,
-- not in Windows.
-- This function fails if encrypted entries are present.
-- See the warning on 'OptPreserveSymbolicLinks' before using it
-- with untrusted archives.
extractFilesFromArchive :: [ZipOption] -> Archive -> IO ()
extractFilesFromArchive opts archive = do
  let entries = zEntries archive
  if OptPreserveSymbolicLinks `elem` opts
    then do
#ifdef _WINDOWS
      mapM_ (writeEntry opts) entries
#else
      let (symbolicLinkEntries, nonSymbolicLinkEntries) = partition isEntrySymbolicLink entries
      mapM_ (writeEntry opts) nonSymbolicLinkEntries
      mapM_ (writeSymbolicLinkEntry opts) symbolicLinkEntries
#endif
    else mapM_ (writeEntry opts) entries

--------------------------------------------------------------------------------
-- Internal functions for reading and writing zip binary format.

-- Note that even on Windows, zip files use "/" internally as path separator.
normalizePath :: FilePath -> String
normalizePath path =
  let dir   = takeDirectory path
      fn    = takeFileName path
      dir' = case dir of
#ifdef _WINDOWS
               (c:':':d:xs) | isLetter c
                            , d == '/' || d == '\\'
                            -> xs  -- remove drive
#endif
               _ -> dir
      -- note: some versions of filepath return ["."] if no dir
      dirParts = filter (/=".") $ splitDirectories dir'
  in  intercalate "/" (dirParts ++ [fn])

-- Equality modulo normalization.  So, "./foo" `matches` "foo".
matches :: FilePath -> FilePath -> Bool
matches fp1 fp2 = normalizePath fp1 == normalizePath fp2

-- | Uncompress a lazy bytestring.
compressData :: CompressionMethod -> B.ByteString -> B.ByteString
compressData Deflate       = Zlib.compress
compressData NoCompression = id

-- | Compress a lazy bytestring.
decompressData :: CompressionMethod -> B.ByteString -> B.ByteString
decompressData Deflate       = Zlib.decompress
decompressData NoCompression = id

-- | Decrypt a lazy bytestring
-- Returns Nothing if password is incorrect or the data is too short
-- to contain the 12-byte encryption header
decryptData :: String -> EncryptionMethod -> B.ByteString -> Maybe B.ByteString
decryptData _ NoEncryption s = Just s
decryptData password (PKWAREEncryption controlByte) s
  | B.length s < headerlen = Nothing
  | otherwise =
      let initKeys = (305419896, 591751049, 878082192)
          startKeys = B.foldl pkwareUpdateKeys initKeys (C.pack password)
          (header, content) = B.splitAt headerlen $ snd $ B.mapAccumL pkwareDecryptByte startKeys s
      in if B.last header == controlByte
            then Just content
            else Nothing
  where headerlen = 12

-- | PKWARE decryption context
type DecryptionCtx = (Word32, Word32, Word32)

-- | An interation of the PKWARE decryption algorithm
pkwareDecryptByte :: DecryptionCtx -> Word8 -> (DecryptionCtx, Word8)
pkwareDecryptByte keys@(_, _, key2) inB =
  let tmp = key2 .|. 2
      tmp' = fromIntegral ((tmp * (tmp `xor` 1)) `shiftR` 8) :: Word8
      outB = inB `xor` tmp'
  in (pkwareUpdateKeys keys outB, outB)

-- | Update decryption keys after a decrypted byte
pkwareUpdateKeys :: DecryptionCtx -> Word8 -> DecryptionCtx
pkwareUpdateKeys (key0, key1, key2) inB =
  let key0' = CRC32.crc32Update (key0 `xor` 0xffffffff) [inB] `xor` 0xffffffff
      key1' = (key1 + (key0' .&. 0xff)) * 134775813 + 1
      key1Byte = fromIntegral (key1' `shiftR` 24) :: Word8
      key2' = CRC32.crc32Update (key2 `xor` 0xffffffff) [key1Byte] `xor` 0xffffffff
  in (key0', key1', key2')

-- | Calculate compression ratio for an entry (for verbose output).
compressionRatio :: Entry -> Float
compressionRatio entry =
  if eUncompressedSize entry == 0
     then 1
     else fromIntegral (eCompressedSize entry) / fromIntegral (eUncompressedSize entry)

-- | MSDOS datetime: a pair of Word16s (date, time) with the following structure:
--
-- > DATE bit     0 - 4           5 - 8           9 - 15
-- >      value   day (1 - 31)    month (1 - 12)  years from 1980
-- > TIME bit     0 - 4           5 - 10          11 - 15
-- >      value   seconds*        minute          hour
-- >              *stored in two-second increments
--
data MSDOSDateTime = MSDOSDateTime { msDOSDate :: Word16
                                   , msDOSTime :: Word16
                                   } deriving (Read, Show, Eq)

-- | Epoch time corresponding to the minimum DOS DateTime (Jan 1 1980 00:00:00).
minMSDOSDateTime :: Integer
minMSDOSDateTime = 315532800

-- | Epoch time corresponding to the maximum DOS DateTime (Dec 31 2107 23:59:58).
maxMSDOSDateTime :: Integer
maxMSDOSDateTime = floor $ utcTimeToPOSIXSeconds $
  UTCTime (fromGregorian 2107 12 31) (23 * 3600 + 59 * 60 + 58)

-- | Convert an epoch time to a MSDOS datetime.  Note that no time zone
-- adjustment happens here: the epoch time is rendered as is, so callers
-- are expected to pass times already shifted to the local time zone
-- (see 'readEntry' and 'setFileTimeStamp').
epochTimeToMSDOSDateTime :: Integer -> MSDOSDateTime
epochTimeToMSDOSDateTime epochtime | epochtime < minMSDOSDateTime =
  epochTimeToMSDOSDateTime minMSDOSDateTime
  -- if time is earlier than minimum DOS datetime, return minimum
epochTimeToMSDOSDateTime epochtime | epochtime > maxMSDOSDateTime =
  epochTimeToMSDOSDateTime maxMSDOSDateTime
  -- if time is later than maximum DOS datetime, return maximum;
  -- the year field of a DOS datetime cannot represent years past 2107,
  -- and larger values would make toEnum fail below
epochTimeToMSDOSDateTime epochtime =
  let
    UTCTime
      (toGregorian -> (fromInteger -> year, month, day))
      (timeToTimeOfDay -> (TimeOfDay hour minutes (floor -> sec)))
      = posixSecondsToUTCTime (fromIntegral epochtime)

    dosTime = toEnum $ (sec `div` 2) + shiftL minutes 5 + shiftL hour 11
    dosDate = toEnum $ day + shiftL month 5 + shiftL (year - 1980) 9
  in  MSDOSDateTime { msDOSDate = dosDate, msDOSTime = dosTime }

-- | Convert a MSDOS datetime to a 'ClockTime'.
msDOSDateTimeToEpochTime :: MSDOSDateTime -> Integer
msDOSDateTimeToEpochTime MSDOSDateTime {msDOSDate = dosDate, msDOSTime = dosTime} =
  let seconds = fromIntegral $ 2 * (dosTime .&. 0O37)
      minutes = fromIntegral $ shiftR dosTime 5 .&. 0O77
      hour    = fromIntegral $ shiftR dosTime 11
      day     = fromIntegral $ dosDate .&. 0O37
      month   = fromIntegral ((shiftR dosDate 5) .&. 0O17)
      year    = fromIntegral $ shiftR dosDate 9
      utc = UTCTime (fromGregorian (1980 + year) month day) (3600 * hour + 60 * minutes + seconds)
  in floor (utcTimeToPOSIXSeconds utc)

#ifndef _WINDOWS
getDirectoryContentsRecursive' :: [ZipOption] -> FilePath -> IO [FilePath]
getDirectoryContentsRecursive' opts path =
  if OptPreserveSymbolicLinks `elem` opts
     then do
       isDir <- doesDirectoryExist path
       if isDir
          then do
            isSymLink <- fmap isSymbolicLink $ getSymbolicLinkStatus path
            if isSymLink
               then return [path]
               else getDirectoryContentsRecursivelyBy (getDirectoryContentsRecursive' opts) path
          else return [path]
     else getDirectoryContentsRecursive path
#endif

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir
     then getDirectoryContentsRecursivelyBy getDirectoryContentsRecursive path
     else return [path]

getDirectoryContentsRecursivelyBy :: (FilePath -> IO [FilePath]) -> FilePath -> IO [FilePath]
getDirectoryContentsRecursivelyBy exploreMethod path = do
       contents <- getDirectoryContents path
       let contents' = map (path </>) $ filter (`notElem` ["..","."]) contents
       children <- mapM exploreMethod contents'
       if path == "."
          then return (concat children)
          else return (path : concat children)


setFileTimeStamp :: FilePath -> Integer -> IO ()
#ifdef _WINDOWS
setFileTimeStamp _ _ = return () -- TODO: figure out how to set the timestamp on Windows
#else
setFileTimeStamp file epochtime = do
  -- eLastModified is relative to the LOCAL time zone (see readEntry
  -- and #67), because MSDOS timestamps are conventionally local time.
  -- Reverse that shift here, so that reading and extracting an entry
  -- preserves the file's modification time.
  tzone <- getTimeZone (posixSecondsToUTCTime (fromIntegral epochtime))
  let epochtime' = fromInteger $
        epochtime - fromIntegral (timeZoneMinutes tzone * 60)
  setFileTimes file epochtime' epochtime'
#endif

-- A zip file has the following format (*'d items are not supported in this implementation):
--
-- >   [local file header 1]
-- >   [file data 1]
-- >   [data descriptor 1*]
-- >   .
-- >   .
-- >   .
-- >   [local file header n]
-- >   [file data n]
-- >   [data descriptor n*]
-- >   [archive decryption header*]
-- >   [archive extra data record*]
-- >   [central directory]
-- >   [zip64 end of central directory record*]
-- >   [zip64 end of central directory locator*]
-- >   [end of central directory record]
--
-- Files stored in arbitrary order.  All values are stored in
-- little-endian byte order unless otherwise specified.
--
--  Central directory structure:
--
-- >   [file header 1]
-- >   .
-- >   .
-- >   .
-- >   [file header n]
-- >   [digital signature]
--
--  End of central directory record:
--
-- >   end of central dir signature    4 bytes  (0x06054b50)
-- >   number of this disk             2 bytes
-- >   number of the disk with the
-- >   start of the central directory  2 bytes
-- >   total number of entries in the
-- >   central directory on this disk  2 bytes
-- >   total number of entries in
-- >   the central directory           2 bytes
-- >   size of the central directory   4 bytes
-- >   offset of start of central
-- >   directory with respect to
-- >   the starting disk number        4 bytes
-- >   .ZIP file comment length        2 bytes
-- >   .ZIP file comment       (variable size)

getArchive :: Get Archive
getArchive = do
  locals <- manySig 0x04034b50 getLocalFile
  files <- manySig 0x02014b50 (getFileHeader (M.fromList locals))
  digSig <- Just `fmap` getDigitalSignature <|> return Nothing
  endSig <- getWord32le
  unless (endSig == 0x06054b50)
    $ fail "Did not find end of central directory signature"
  skip 2 -- disk number
  skip 2 -- disk number of central directory
  skip 2 -- num entries on this disk
  skip 2 -- num entries in central directory
  skip 4 -- central directory size
  skip 4 -- offset of central directory
  commentLength <- getWord16le
  zipComment <- getLazyByteString (toEnum $ fromEnum commentLength)
  return Archive
           { zEntries                = files
           , zSignature              = digSig
           , zComment                = zipComment
           }

putArchive :: Archive -> Put
putArchive archive = do
  let numEntries = length $ zEntries archive
  when (numEntries >= 0xFFFF) $
    E.throw $ Zip64NotSupported "65535 or more entries require ZIP64"
  mapM_ putLocalFile $ zEntries archive
  let localFileSizes = map localFileSize $ zEntries archive
  let offsets = scanl (+) 0 localFileSizes
  let cdOffset = last offsets
  when (cdOffset >= 0xFFFFFFFF) $
    E.throw $ Zip64NotSupported "archive of 4GB or more requires ZIP64"
  _ <- zipWithM_ putFileHeader (map fromIntegral offsets) (zEntries archive)
  putDigitalSignature $ zSignature archive
  putWord32le 0x06054b50
  putWord16le 0 -- disk number
  putWord16le 0 -- disk number of central directory
  putWord16le $ fromIntegral $ length $ zEntries archive -- number of entries this disk
  putWord16le $ fromIntegral $ length $ zEntries archive -- number of entries
  putWord32le $ sum $ map fileHeaderSize $ zEntries archive  -- size of central directory
  putWord32le $ fromIntegral cdOffset                    -- offset of central dir
  putWord16le $ fromIntegral $ B.length $ zComment archive
  putLazyByteString $ zComment archive


fileHeaderSize :: Entry -> Word32
fileHeaderSize f =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 +
    fromIntegral (B.length $ fromString $ normalizePath $ eRelativePath f) +
    B.length (eExtraField f) + B.length (eFileComment f)

-- Note: computed as Int64 (not Word32) so that putArchive can detect
-- offsets that would overflow the 32-bit fields of the zip format.
localFileSize :: Entry -> Int64
localFileSize f =
  4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 +
    B.length (fromString $ normalizePath $ eRelativePath f) +
    B.length (eExtraField f) + B.length (eCompressedData f)

-- Local file header:
--
-- >    local file header signature     4 bytes  (0x04034b50)
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
--
-- Note that if bit 3 of the general purpose bit flag is set, then the
-- compressed size will be 0 and the size will be stored instead in a
-- data descriptor record AFTER the file contents. The record normally
-- begins with the signature 0x08074b50, then 4 bytes crc-32, 4 bytes
-- compressed size, 4 bytes uncompressed size.

getLocalFile :: Get (Word32, B.ByteString)
getLocalFile = do
  offset <- bytesRead
  getWord32le >>= ensure (== 0x04034b50)
  skip 2  -- version
  bitflag <- getWord16le
  rawCompressionMethod <- getWord16le
  compressionMethod <- case rawCompressionMethod of
                        0 -> return NoCompression
                        8 -> return Deflate
                        _ -> fail $ "Unknown compression method " ++ show rawCompressionMethod
  skip 2  -- last mod file time
  skip 2  -- last mod file date
  skip 4  -- crc32
  compressedSize <- getWord32le
  when (compressedSize == 0xFFFFFFFF) $
    fail "Can't read ZIP64 archive."
  skip 4  -- uncompressedsize
  fileNameLength <- getWord16le
  extraFieldLength <- getWord16le
  skip (fromIntegral fileNameLength)  -- filename
  skip (fromIntegral extraFieldLength) -- extra field
  compressedData <-
    if bitflag .&. 0O10 == 0
      then getLazyByteString (fromIntegral compressedSize)
      else -- If bit 3 of general purpose bit flag is set,
           -- then we need to read until we get to the
           -- data descriptor record.
           do raw <- getCompressedData compressionMethod
              sig <- lookAhead getWord32le
              when (sig == 0x08074b50) $ skip 4
              skip 4 -- crc32
              cs <- getWord32le  -- compressed size
              skip 4 -- uncompressed size
              if fromIntegral cs == B.length raw
                 then return raw
                 else fail $ printf
                       ("Content size mismatch in data descriptor record: "
                         ++ "expected %d, got %d bytes")
                       cs (B.length raw)
  return (fromIntegral offset, compressedData)

putLocalFile :: Entry -> Put
putLocalFile f = do
  putWord32le 0x04034b50
  putWord16le 20 -- version needed to extract (>=2.0)
  putWord16le 0x802  -- general purpose bit flag (bit 1 = max compression, bit 11 = UTF-8)
  putWord16le $ case eCompressionMethod f of
                     NoCompression -> 0
                     Deflate       -> 8
  let modTime = epochTimeToMSDOSDateTime $ eLastModified f
  putWord16le $ msDOSTime modTime
  putWord16le $ msDOSDate modTime
  putWord32le $ eCRC32 f
  putWord32le $ eCompressedSize f
  putWord32le $ eUncompressedSize f
  putWord16le $ fromIntegral $ B.length $ fromString
              $ normalizePath $ eRelativePath f
  putWord16le $ fromIntegral $ B.length $ eExtraField f
  putLazyByteString $ fromString $ normalizePath $ eRelativePath f
  putLazyByteString $ eExtraField f
  putLazyByteString $ eCompressedData f

-- File header structure:
--
-- >    central file header signature   4 bytes  (0x02014b50)
-- >    version made by                 2 bytes
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
-- >    file comment length             2 bytes
-- >    disk number start               2 bytes
-- >    internal file attributes        2 bytes
-- >    external file attributes        4 bytes
-- >    relative offset of local header 4 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
-- >    file comment (variable size)

getFileHeader :: M.Map Word32 B.ByteString -- ^ map of (offset, content) pairs returned by getLocalFile
              -> Get Entry
getFileHeader locals = do
  getWord32le >>= ensure (== 0x02014b50)
  vmb <- getWord16le  -- version made by
  versionNeededToExtract <- getWord8
  skip 1 -- upper byte indicates OS part of "version needed to extract"
  unless (versionNeededToExtract <= 20) $
    fail "This archive requires zip >= 2.0 to extract."
  bitflag <- getWord16le
  rawCompressionMethod <- getWord16le
  compressionMethod <- case rawCompressionMethod of
                        0 -> return NoCompression
                        8 -> return Deflate
                        _ -> fail $ "Unknown compression method " ++ show rawCompressionMethod
  lastModFileTime <- getWord16le
  lastModFileDate <- getWord16le
  crc32 <- getWord32le
  encryptionMethod <- case (testBit bitflag 0, testBit bitflag 3, testBit bitflag 6) of
                        (False, _, _) -> return NoEncryption
                        (True, False, False) -> return $ PKWAREEncryption (fromIntegral (crc32 `shiftR` 24))
                        (True, True, False) -> return $ PKWAREEncryption (fromIntegral (lastModFileTime `shiftR` 8))
                        (True, _, True) -> fail "Strong encryption is not supported"

  compressedSize <- getWord32le
  uncompressedSize <- getWord32le
  fileNameLength <- getWord16le
  extraFieldLength <- getWord16le
  fileCommentLength <- getWord16le
  skip 2 -- disk number start
  internalFileAttributes <- getWord16le
  externalFileAttributes <- getWord32le
  relativeOffset <- getWord32le
  fileName <- getLazyByteString (toEnum $ fromEnum fileNameLength)
  extraField <- getLazyByteString (toEnum $ fromEnum extraFieldLength)
  fileComment <- getLazyByteString (toEnum $ fromEnum fileCommentLength)
  compressedData <- case M.lookup relativeOffset locals of
                    Just x  -> return x
                    Nothing -> fail $ "Unable to find data at offset " ++
                                        show relativeOffset
  return Entry
            { eRelativePath            = decodeFileName bitflag fileName
            , eCompressionMethod       = compressionMethod
            , eEncryptionMethod        = encryptionMethod
            , eLastModified            = msDOSDateTimeToEpochTime $
                                         MSDOSDateTime { msDOSDate = lastModFileDate,
                                                         msDOSTime = lastModFileTime }
            , eCRC32                   = crc32
            , eCompressedSize          = compressedSize
            , eUncompressedSize        = uncompressedSize
            , eExtraField              = extraField
            , eFileComment             = fileComment
            , eVersionMadeBy           = vmb
            , eInternalFileAttributes  = internalFileAttributes
            , eExternalFileAttributes  = externalFileAttributes
            , eCompressedData          = compressedData
            }

putFileHeader :: Word32        -- ^ offset
              -> Entry
              -> Put
putFileHeader offset local = do
  putWord32le 0x02014b50
  putWord16le $ eVersionMadeBy local
  putWord16le 20 -- version needed to extract (>= 2.0)
  putWord16le 0x802  -- general purpose bit flag (bit 1 = max compression, bit 11 = UTF-8)
  putWord16le $ case eCompressionMethod local of
                     NoCompression -> 0
                     Deflate       -> 8
  let modTime = epochTimeToMSDOSDateTime $ eLastModified local
  putWord16le $ msDOSTime modTime
  putWord16le $ msDOSDate modTime
  putWord32le $ eCRC32 local
  putWord32le $ eCompressedSize local
  putWord32le $ eUncompressedSize local
  putWord16le $ fromIntegral $ B.length $ fromString
              $ normalizePath $ eRelativePath local
  putWord16le $ fromIntegral $ B.length $ eExtraField local
  putWord16le $ fromIntegral $ B.length $ eFileComment local
  putWord16le 0  -- disk number start
  putWord16le $ eInternalFileAttributes local
  putWord32le $ eExternalFileAttributes local
  putWord32le offset
  putLazyByteString $ fromString $ normalizePath $ eRelativePath local
  putLazyByteString $ eExtraField local
  putLazyByteString $ eFileComment local

--  Digital signature:
--
-- >     header signature                4 bytes  (0x05054b50)
-- >     size of data                    2 bytes
-- >     signature data (variable size)

getDigitalSignature :: Get B.ByteString
getDigitalSignature = do
  getWord32le >>= ensure (== 0x05054b50)
  sigSize <- getWord16le
  getLazyByteString (toEnum $ fromEnum sigSize)

putDigitalSignature :: Maybe B.ByteString -> Put
putDigitalSignature Nothing = return ()
putDigitalSignature (Just sig) = do
  putWord32le 0x05054b50
  putWord16le $ fromIntegral $ B.length sig
  putLazyByteString sig

ensure :: (a -> Bool) -> a -> Get ()
ensure p val =
  if p val
     then return ()
     else fail "ensure not satisfied"

-- | Decode a file name from a zip archive according to the general
-- purpose bit flag: if bit 11 is set, the name is UTF-8 encoded;
-- otherwise the zip spec says it is encoded in IBM code page 437.
-- Invalid UTF-8 is decoded leniently (invalid bytes are replaced by
-- U+FFFD) rather than raising an exception, so that 'toArchiveOrFail'
-- remains total.
decodeFileName :: Word16 -> B.ByteString -> String
decodeFileName bitflag fn
  | testBit bitflag 11 = TL.unpack $ TL.decodeUtf8With TE.lenientDecode fn
  | otherwise          = map cp437ToChar $ B.unpack fn

cp437ToChar :: Word8 -> Char
cp437ToChar w
  | w < 128   = toEnum (fromIntegral w)
  | otherwise = cp437table !! fromIntegral (w - 128)

-- IBM code page 437, upper half (0x80 - 0xFF).
cp437table :: String
cp437table =
  "\199\252\233\226\228\224\229\231\234\235\232\239\238\236\196\197\
  \\201\230\198\244\246\242\251\249\255\214\220\162\163\165\8359\402\
  \\225\237\243\250\241\209\170\186\191\8976\172\189\188\161\171\187\
  \\9617\9618\9619\9474\9508\9569\9570\9558\9557\9571\9553\9559\9565\9564\9563\9488\
  \\9492\9524\9516\9500\9472\9532\9566\9567\9562\9556\9577\9574\9568\9552\9580\9575\
  \\9576\9572\9573\9561\9560\9554\9555\9579\9578\9496\9484\9608\9604\9612\9616\9600\
  \\945\223\915\960\931\963\181\964\934\920\937\948\8734\966\949\8745\
  \\8801\177\8805\8804\8992\8993\247\8776\176\8729\183\8730\8319\178\9632\160"

fromString :: String -> B.ByteString
fromString = TL.encodeUtf8 . TL.pack

data DecompressResult =
    DecompressSuccess B.ByteString -- bytes remaining
      -- (we just discard decompressed chunks, because we only
      -- want to know where the compressed data ends)
  | DecompressFailure ZlibInt.DecompressError

getCompressedData :: CompressionMethod -> Get B.ByteString
getCompressedData NoCompression = do
  -- we assume there will be a signature on the data descriptor,
  -- otherwise we have no way of identifying where the data ends!
  -- The signature 0x08074b50 is commonly used but not required by spec.
  let findSigPos = do
        w1 <- getWord8
        if w1 == 0x50
           then do
             w2 <- getWord8
             if w2 == 0x4b
                then do
                  w3 <- getWord8
                  if w3 == 0x07
                     then do
                       w4 <- getWord8
                       if w4 == 0x08
                          then (\x -> x - 4) <$> bytesRead
                          else findSigPos
                     else findSigPos
                else findSigPos
           else findSigPos
  pos <- bytesRead
  sigpos <- lookAhead findSigPos <|>
              fail "getCompressedData can't find data descriptor signature"
  let compressedBytes = sigpos - pos
  getLazyByteString compressedBytes
getCompressedData Deflate = do
  remainingBytes <- lookAhead getRemainingLazyByteString
  let result = ZlibInt.foldDecompressStreamWithInput
                (\_bs res -> res)
                DecompressSuccess
                DecompressFailure
                (ZlibInt.decompressST ZlibInt.rawFormat
                 ZlibInt.defaultDecompressParams{
                     ZlibInt.decompressAllMembers = False })
                remainingBytes
  case result of
    DecompressFailure err -> fail (show err)
    DecompressSuccess afterCompressedBytes ->
      -- Consume the compressed bytes; we don't do anything with
      -- the decompressed chunks. We are just decompressing as a
      -- way of finding where the compressed data ends.
      getLazyByteString
        (fromIntegral (B.length remainingBytes - B.length afterCompressedBytes))

