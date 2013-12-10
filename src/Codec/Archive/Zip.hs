{-# LANGUAGE CPP #-}
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
-- particular, there is no support for encryption, zip files that span
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
       , ZipOption (..)
       , emptyArchive

       -- * Pure functions for working with zip archives
       , toArchive
       , fromArchive
       , filesInArchive
       , addEntryToArchive
       , deleteEntryFromArchive
       , findEntryByPath
       , fromEntry
       , toEntry

       -- * IO functions for working with zip archives
       , readEntry
       , writeEntry
       , addFilesToArchive
       , extractFilesFromArchive

       ) where

import System.Time ( toUTCTime, addToClockTime, CalendarTime (..), ClockTime (..), TimeDiff (..) )
#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
#else
#endif
import Data.Bits ( shiftL, shiftR, (.&.) )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List ( nub, find )
import Text.Printf
import System.FilePath
import System.Directory ( doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing )
import Control.Monad ( when, unless, zipWithM )
#if MIN_VERSION_directory(1,2,0)
import Control.Monad ( liftM )
#endif
import System.Directory ( getModificationTime )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as M
#if MIN_VERSION_binary(0, 6, 0)
import Control.Applicative
#endif
#ifndef _WINDOWS
import System.Posix.Files ( setFileTimes )
#endif

-- from bytestring
import qualified Data.ByteString.Lazy as B

-- from utf8-string
import Data.ByteString.Lazy.UTF8 ( toString, fromString )

-- from zlib
import qualified Codec.Compression.Zlib.Raw as Zlib

#if !MIN_VERSION_binary(0, 6, 0)
manySig :: Word32 -> Get a -> Get [a]
manySig sig p = do
    sig' <- lookAhead getWord32le
    if sig == sig'
        then do
            r <- p
            rs <- manySig sig p
            return $ r : rs
        else return []
#endif

------------------------------------------------------------------------

-- | Structured representation of a zip archive, including directory
-- information and contents (in lazy bytestrings).
data Archive = Archive
                { zEntries                :: [Entry]              -- ^ Files in zip archive
                , zSignature              :: Maybe B.ByteString   -- ^ Digital signature
                , zComment                :: B.ByteString         -- ^ Comment for whole zip archive
                } deriving (Read, Show)

instance Binary Archive where
  put = putArchive
  get = getArchive

-- | Representation of an archived file, including content and metadata.
data Entry = Entry
               { eRelativePath            :: FilePath            -- ^ Relative path, using '/' as separator
               , eCompressionMethod       :: CompressionMethod   -- ^ Compression method
               , eLastModified            :: Integer             -- ^ Modification time (seconds since unix epoch)
               , eCRC32                   :: Word32              -- ^ CRC32 checksum
               , eCompressedSize          :: Word32              -- ^ Compressed size in bytes
               , eUncompressedSize        :: Word32              -- ^ Uncompressed size in bytes
               , eExtraField              :: B.ByteString        -- ^ Extra field - unused by this library
               , eFileComment             :: B.ByteString        -- ^ File comment - unused by this library
               , eInternalFileAttributes  :: Word16              -- ^ Internal file attributes - unused by this library
               , eExternalFileAttributes  :: Word32              -- ^ External file attributes (system-dependent)
               , eCompressedData          :: B.ByteString        -- ^ Compressed contents of file
               } deriving (Read, Show, Eq)

-- | Compression methods.
data CompressionMethod = Deflate
                       | NoCompression
                       deriving (Read, Show, Eq)

-- | Options for 'addFilesToArchive' and 'extractFilesFromArchive'.
data ZipOption = OptRecursive               -- ^ Recurse into directories when adding files
               | OptVerbose                 -- ^ Print information to stderr
               | OptDestination FilePath    -- ^ Directory in which to extract
               deriving (Read, Show, Eq)

-- | A zip archive with no contents.
emptyArchive :: Archive
emptyArchive = Archive
                { zEntries                  = []
                , zSignature              = Nothing
                , zComment                = B.empty }

-- | Reads an 'Archive' structure from a raw zip archive (in a lazy bytestring).
toArchive :: B.ByteString -> Archive
toArchive = decode

-- | Writes an 'Archive' structure to a raw zip archive (in a lazy bytestring).
fromArchive :: Archive -> B.ByteString
fromArchive = encode

-- | Returns a list of files in a zip archive.
filesInArchive :: Archive -> [FilePath]
filesInArchive = (map eRelativePath) . zEntries

-- | Adds an entry to a zip archive, or updates an existing entry.
addEntryToArchive :: Entry -> Archive -> Archive
addEntryToArchive entry archive =
  let archive'   = deleteEntryFromArchive (eRelativePath entry) archive
      oldEntries = zEntries archive'
  in  archive' { zEntries = entry : oldEntries }

-- | Deletes an entry from a zip archive.
deleteEntryFromArchive :: FilePath -> Archive -> Archive
deleteEntryFromArchive path archive =
  let path'      = zipifyFilePath path
      newEntries = filter (\e -> eRelativePath e /= path') $ zEntries archive
  in  archive { zEntries = newEntries }

-- | Returns Just the zip entry with the specified path, or Nothing.
findEntryByPath :: FilePath -> Archive -> Maybe Entry
findEntryByPath path archive = find (\e -> path == eRelativePath e) (zEntries archive)

-- | Returns uncompressed contents of zip entry.
fromEntry :: Entry -> B.ByteString
fromEntry entry =
  let uncompressedData = decompressData (eCompressionMethod entry) (eCompressedData entry)
  in  if eCRC32 entry == CRC32.crc32 uncompressedData
         then uncompressedData
         else error "CRC32 mismatch"

-- | Create an 'Entry' with specified file path, modification time, and contents.
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
  in  Entry { eRelativePath            = path
            , eCompressionMethod       = compressionMethod
            , eLastModified            = modtime
            , eCRC32                   = crc32
            , eCompressedSize          = fromIntegral finalSize
            , eUncompressedSize        = fromIntegral uncompressedSize
            , eExtraField              = B.empty
            , eFileComment             = B.empty
            , eInternalFileAttributes  = 0  -- potentially non-text
            , eExternalFileAttributes  = 0  -- appropriate if from stdin
            , eCompressedData          = finalData
            }

-- | Generates a 'Entry' from a file or directory.
readEntry :: [ZipOption] -> FilePath -> IO Entry
readEntry opts path = do
  isDir <- doesDirectoryExist path
  let path' = zipifyFilePath $ normalise $
              path ++ if isDir then "/" else ""  -- make sure directories end with /
  contents <- if isDir
                 then return B.empty
                 else B.readFile path
#if MIN_VERSION_directory(1,2,0)
  modEpochTime <- liftM (floor . utcTimeToPOSIXSeconds)
                   $ getModificationTime path
#else
  (TOD modEpochTime _) <- getModificationTime path
#endif
  let entry = toEntry path' modEpochTime contents
  when (OptVerbose `elem` opts) $ do
    let compmethod = case eCompressionMethod entry of
                     Deflate       -> "deflated"
                     NoCompression -> "stored"
    hPutStrLn stderr $
      printf "  adding: %s (%s %.f%%)" (eRelativePath entry)
      compmethod (100 - (100 * compressionRatio entry))
  return entry

-- | Writes contents of an 'Entry' to a file.
writeEntry :: [ZipOption] -> Entry -> IO ()
writeEntry opts entry = do
  let path = case [d | OptDestination d <- opts] of
                  (x:_) -> x </> eRelativePath entry
                  _     -> eRelativePath entry
  -- create directories if needed
  let dir = takeDirectory path
  exists <- doesDirectoryExist dir
  unless exists $ do
    createDirectoryIfMissing True dir
    when (OptVerbose `elem` opts) $
      hPutStrLn stderr $ "  creating: " ++ dir
  if length path > 0 && last path == '/' -- path is a directory
     then return ()
     else do
       when (OptVerbose `elem` opts) $ do
         hPutStrLn stderr $ case eCompressionMethod entry of
                                 Deflate       -> " inflating: " ++ path
                                 NoCompression -> "extracting: " ++ path
       B.writeFile path (fromEntry entry)
  -- Note that last modified times are supported only for POSIX, not for
  -- Windows.
  setFileTimeStamp path (eLastModified entry)

-- | Add the specified files to an 'Archive'.  If 'OptRecursive' is specified,
-- recursively add files contained in directories.  If 'OptVerbose' is specified,
-- print messages to stderr.
addFilesToArchive :: [ZipOption] -> Archive -> [FilePath] -> IO Archive
addFilesToArchive opts archive files = do
  filesAndChildren <- if OptRecursive `elem` opts
                         then mapM getDirectoryContentsRecursive files >>= return . nub . concat
                         else return files
  entries <- mapM (readEntry opts) filesAndChildren
  return $ foldr addEntryToArchive archive entries

-- | Extract all files from an 'Archive', creating directories
-- as needed.  If 'OptVerbose' is specified, print messages to stderr.
-- Note that the last-modified time is set correctly only in POSIX,
-- not in Windows.
extractFilesFromArchive :: [ZipOption] -> Archive -> IO ()
extractFilesFromArchive opts archive =
  mapM_ (writeEntry opts) $ zEntries archive

--------------------------------------------------------------------------------
-- Internal functions for reading and writing zip binary format.

-- Note that even on Windows, zip files use "/" internally as path separator.
zipifyFilePath :: FilePath -> String
zipifyFilePath path =
  let dir = takeDirectory path
      fn  = takeFileName path
      (_drive, dir') = splitDrive dir
      -- note: some versions of filepath return ["."] if no dir
      dirParts = dropWhile (==".") $ splitDirectories dir'
  in  (concat (map (++ "/") dirParts)) ++ fn

-- | Uncompress a lazy bytestring.
compressData :: CompressionMethod -> B.ByteString -> B.ByteString
compressData Deflate       = Zlib.compress
compressData NoCompression = id

-- | Compress a lazy bytestring.
decompressData :: CompressionMethod -> B.ByteString -> B.ByteString
decompressData Deflate       = Zlib.decompress
decompressData NoCompression = id

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

-- | Convert a clock time to a MSDOS datetime.  The MSDOS time will be relative to UTC.
epochTimeToMSDOSDateTime :: Integer -> MSDOSDateTime
epochTimeToMSDOSDateTime epochtime | epochtime < minMSDOSDateTime =
  epochTimeToMSDOSDateTime minMSDOSDateTime
  -- if time is earlier than minimum DOS datetime, return minimum
epochTimeToMSDOSDateTime epochtime =
  let ut = toUTCTime (TOD epochtime 0)
      dosTime = toEnum $ (ctSec ut `div` 2) + shiftL (ctMin ut) 5 + shiftL (ctHour ut) 11
      dosDate = toEnum $ ctDay ut + shiftL (fromEnum (ctMonth ut) + 1) 5 + shiftL (ctYear ut - 1980) 9
  in  MSDOSDateTime { msDOSDate = dosDate, msDOSTime = dosTime }

-- | Convert a MSDOS datetime to a 'ClockTime'.
msDOSDateTimeToEpochTime :: MSDOSDateTime -> Integer
msDOSDateTimeToEpochTime (MSDOSDateTime {msDOSDate = dosDate, msDOSTime = dosTime}) =
  let seconds = fromIntegral $ 2 * (dosTime .&. 0O37)
      minutes = fromIntegral $ (shiftR dosTime 5) .&. 0O77
      hour    = fromIntegral $ shiftR dosTime 11
      day     = fromIntegral $ dosDate .&. 0O37
      month   = fromIntegral $ ((shiftR dosDate 5) .&. 0O17) - 1
      year    = fromIntegral $ shiftR dosDate 9
      timeSinceEpoch = TimeDiff
               { tdYear = year + 10, -- dos times since 1980, unix epoch starts 1970
                 tdMonth = month,
                 tdDay = day - 1,  -- dos days start from 1
                 tdHour = hour,
                 tdMin = minutes,
                 tdSec = seconds,
                 tdPicosec = 0 }
      (TOD epochsecs _) = addToClockTime timeSinceEpoch (TOD 0 0)
  in  epochsecs

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       contents <- getDirectoryContents path
       let contents' = map (path </>) $ filter (`notElem` ["..","."]) contents
       children <- mapM getDirectoryContentsRecursive contents'
       if path == "."
          then return (concat children)
          else return (path : concat children)
     else return [path]

setFileTimeStamp :: FilePath -> Integer -> IO ()
setFileTimeStamp file epochtime = do
#ifdef _WINDOWS
  return ()  -- TODO - figure out how to set the timestamp on Windows
#else
  let epochtime' = fromInteger epochtime
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
#if MIN_VERSION_binary(0, 6, 0)
  locals <- many getLocalFile
  files <- many (getFileHeader (M.fromList locals))
  digSig <- Just `fmap` getDigitalSignature <|> return Nothing
#else
  locals <- manySig 0x04034b50 getLocalFile
  files <- manySig 0x02014b50 (getFileHeader (M.fromList locals))
  digSig <- lookAheadM getDigitalSignature
#endif
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
  return $ Archive
           { zEntries                = files
           , zSignature              = digSig
           , zComment                = zipComment
           }

putArchive :: Archive -> Put
putArchive archive = do
  mapM_ putLocalFile $ zEntries archive
  let localFileSizes = map localFileSize $ zEntries archive
  let offsets = scanl (+) 0 localFileSizes
  let cdOffset = last offsets
  _ <- zipWithM putFileHeader offsets (zEntries archive)
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
    fromIntegral (B.length $ fromString $ zipifyFilePath $ eRelativePath f) +
    B.length (eExtraField f) + B.length (eFileComment f)

localFileSize :: Entry -> Word32
localFileSize f =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 +
    fromIntegral (B.length $ fromString $ zipifyFilePath $ eRelativePath f) +
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
  skip 2  -- compressionMethod
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
  compressedData <- if bitflag .&. 0O10 == 0
      then getLazyByteString (fromIntegral compressedSize)
      else -- If bit 3 of general purpose bit flag is set,
           -- then we need to read until we get to the
           -- data descriptor record.  We assume that the
           -- record has signature 0x08074b50; this is not required
           -- by the specification but is common.
           do raw <- getWordsTilSig 0x08074b50
              skip 4 -- crc32
              cs <- getWord32le  -- compressed size
              skip 4 -- uncompressed size
              if fromIntegral cs == length raw
                 then return $ B.pack raw
                 else fail "Content size mismatch in data descriptor record" 
  return (fromIntegral offset, compressedData)

getWordsTilSig :: Word32 -> Get [Word8]
getWordsTilSig sig = do
#if MIN_VERSION_binary(0, 6, 0)
  (getWord32le >>= ensure (== sig) >> return []) <|>
    do w <- getWord8
       ws <- getWordsTilSig sig
       return (w:ws)
#else
    sig' <- lookAhead getWord32le
    if sig == sig'
        then skip 4 >> return []
        else do
            w <- getWord8
            ws <- getWordsTilSig sig
            return (w:ws)
#endif

putLocalFile :: Entry -> Put
putLocalFile f = do
  putWord32le 0x04034b50
  putWord16le 20 -- version needed to extract (>=2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
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
              $ zipifyFilePath $ eRelativePath f
  putWord16le $ fromIntegral $ B.length $ eExtraField f
  putLazyByteString $ fromString $ zipifyFilePath $ eRelativePath f
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
  skip 2 -- version made by
  versionNeededToExtract <- getWord8
  skip 1 -- upper byte indicates OS part of "version needed to extract"
  unless (versionNeededToExtract <= 20) $
    fail "This archive requires zip >= 2.0 to extract."
  skip 2 -- general purpose bit flag
  rawCompressionMethod <- getWord16le
  compressionMethod <- case rawCompressionMethod of
                        0 -> return NoCompression
                        8 -> return Deflate
                        _ -> fail $ "Unknown compression method " ++ show rawCompressionMethod
  lastModFileTime <- getWord16le
  lastModFileDate <- getWord16le
  crc32 <- getWord32le
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
  compressedData <- case (M.lookup relativeOffset locals) of
                    Just x  -> return x
                    Nothing -> fail $ "Unable to find data at offset " ++
                                        show relativeOffset
  return $ Entry
            { eRelativePath            = toString fileName
            , eCompressionMethod       = compressionMethod
            , eLastModified            = msDOSDateTimeToEpochTime $
                                         MSDOSDateTime { msDOSDate = lastModFileDate,
                                                         msDOSTime = lastModFileTime }
            , eCRC32                   = crc32
            , eCompressedSize          = compressedSize
            , eUncompressedSize        = uncompressedSize
            , eExtraField              = extraField
            , eFileComment             = fileComment
            , eInternalFileAttributes  = internalFileAttributes
            , eExternalFileAttributes  = externalFileAttributes
            , eCompressedData          = compressedData
            }

putFileHeader :: Word32        -- ^ offset
              -> Entry
              -> Put
putFileHeader offset local = do
  putWord32le 0x02014b50
  putWord16le 0  -- version made by
  putWord16le 20 -- version needed to extract (>= 2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
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
              $ zipifyFilePath $ eRelativePath local
  putWord16le $ fromIntegral $ B.length $ eExtraField local
  putWord16le $ fromIntegral $ B.length $ eFileComment local
  putWord16le 0  -- disk number start
  putWord16le $ eInternalFileAttributes local
  putWord32le $ eExternalFileAttributes local
  putWord32le offset
  putLazyByteString $ fromString $ zipifyFilePath $ eRelativePath local
  putLazyByteString $ eExtraField local
  putLazyByteString $ eFileComment local

--  Digital signature:
--
-- >     header signature                4 bytes  (0x05054b50)
-- >     size of data                    2 bytes
-- >     signature data (variable size)

#if MIN_VERSION_binary(0, 6, 0)
getDigitalSignature :: Get B.ByteString
getDigitalSignature = do
  getWord32le >>= ensure (== 0x05054b50)
  sigSize <- getWord16le
  getLazyByteString (toEnum $ fromEnum sigSize)
#else
getDigitalSignature :: Get (Maybe B.ByteString)
getDigitalSignature = do
  hdrSig <- getWord32le
  if hdrSig /= 0x05054b50
     then return Nothing
     else do
        sigSize <- getWord16le
        getLazyByteString (toEnum $ fromEnum sigSize) >>= return . Just
#endif

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
