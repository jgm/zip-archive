{-# LANGUAGE CPP #-}
------------------------------------------------------------------------
-- |
-- Module      : Codec.Archive.Zip
-- Copyright   : John MacFarlane
-- License     : GPL 2 (see LICENSE)
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
-- multiple disks, ZIP64, or compression methods other than Deflate.
-- However, the library should be able to read the most common zip
-- archives, and the archives it produces should be readable by all
-- standard unzip programs.
--
-- Because the file modification times in zip archives do not include
-- time zone information, UTC is assumed.
--
-- As an example of the use of the library, a standalone zip archiver
-- and extracter, Zip.hs, is provided in the source distribution.
--
-- For more information on the format of zip archives, consult
-- <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>
------------------------------------------------------------------------

module Codec.Archive.Zip
       (

       -- * Data structures and default values
         ZipArchive (..)
       , ZipEntry (..)
       , CompressionMethod (..)
       , ZipOptions (..)
       , emptyZipArchive
       , defaultZipEntry

       -- * Functions for working with zip archives
       , toZipArchive
       , fromZipArchive
       , readZipArchive
       , writeZipArchive
       , filesInZipArchive
       , addEntryToZipArchive
       , deleteEntryFromZipArchive

       -- * Functions for working with zip entries
       , findZipEntryByPath
       , contentsOfZipEntry
       , readZipEntry
       , writeZipEntry

       -- * Functions for compressing and decompressing data
       , compressData
       , decompressData

       -- * IO functions for adding files to and extracting files from a zip archive
       , addFilesToZipArchive
       , extractFilesFromZipArchive

       -- * Conversion of MSDOS datetime used in zip archives
       , MSDOSDateTime (..)
       , clockTimeToMSDOSDateTime
       , msDOSDateTimeToClockTime

       ) where

import System.Time ( toUTCTime, toClockTime, addToClockTime,
                     CalendarTime (..), ClockTime (..), TimeDiff (..), Day (..), Month (..) )
import Data.Bits ( shiftL, shiftR, (.&.) )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List ( nub, find )
import Data.Maybe
import Text.Printf
import System.FilePath
import System.Directory ( doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing )
import qualified Control.Monad.State as S
import Control.Monad ( when, unless, zipWithM )
import System.Directory ( getModificationTime )
import System.IO ( stderr, hPutStrLn )

#ifndef _WINDOWS
import System.Posix.Files ( setFileTimes )
#endif

-- from bytestring
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Internal ( w2c )

-- from utf8-string
import Data.ByteString.Lazy.UTF8 ( toString, fromString )

-- from zlib
import qualified Codec.Compression.Zlib.Raw as Zlib

import qualified Data.Hash.CRC32.GZip as CRC32

------------------------------------------------------------------------
-- | Structured representation of a zip archive, including directory
-- information and contents (in lazy bytestrings).

data ZipArchive = ZipArchive
                { zEntries                :: [ZipEntry]           -- ^ files in zip archive
                , zSignature              :: Maybe B.ByteString   -- ^ digital signature
                , zComment                :: B.ByteString         -- ^ comment for whole zip archive
                } deriving (Read, Show, Eq)

-- | Representation of an archived file, including content and metadata.
data ZipEntry = ZipEntry
               { eRelativePath            :: FilePath            -- ^ relative path, using '/' as separator
               , eCompressionMethod       :: CompressionMethod   -- ^ compression method
               , eLastModified            :: MSDOSDateTime       -- ^ date and time last modified
               , eCRC32                   :: Word32              -- ^ crc32 checksum
               , eCompressedSize          :: Word32              -- ^ compressed size in bytes
               , eUncompressedSize        :: Word32              -- ^ uncompressed size in bytes
               , eExtraField              :: B.ByteString        -- ^ extra field - unused by this library
               , eFileComment             :: B.ByteString        -- ^ file comment - unused by this library
               , eInternalFileAttributes  :: Word16              -- ^ internal file attributes - unused by this library
               , eExternalFileAttributes  :: Word32              -- ^ external file attributes (system-dependent)
               , eCompressedData          :: B.ByteString        -- ^ compressed contents of file
               } deriving (Read, Show, Eq)

-- | Compression methods.
data CompressionMethod = Deflate
                       | NoCompression
                       deriving (Read, Show, Eq)

-- | Options for 'addFilesToZipArchive' and 'extractFilesFromZipArchive'.
data ZipOptions = OptRecursive | OptVerbose deriving (Read, Show, Eq)

-- | A zip archive with no contents.
emptyZipArchive :: ZipArchive
emptyZipArchive = ZipArchive
                { zEntries                  = []
                , zSignature              = Nothing
                , zComment                = B.empty }

-- | Default values for a 'ZipEntry'.
defaultZipEntry :: ZipEntry
defaultZipEntry = ZipEntry
                 { eRelativePath            = ""
                 , eCompressionMethod       = Deflate
                 , eLastModified            = clockTimeToMSDOSDateTime (toClockTime epoch)
                 , eCRC32                   = 0
                 , eCompressedSize          = 0
                 , eUncompressedSize        = 0
                 , eExtraField              = B.empty
                 , eFileComment             = B.empty
                 , eInternalFileAttributes  = 0  -- potentially non-text
                 , eExternalFileAttributes  = 0  -- appropriate if from stdin
                 , eCompressedData          = B.empty
                 }

-- | Reads a 'ZipArchive' structure from a raw zip archive (in a lazy bytestring).
toZipArchive :: B.ByteString -> ZipArchive
toZipArchive = runGet getZipArchive

-- | Writes a 'ZipArchive' structure to a raw zip archive (in a lazy bytestring).
fromZipArchive :: ZipArchive -> B.ByteString
fromZipArchive = runPut . putZipArchive

-- | Reads a 'ZipArchive' structure from a zip file.
readZipArchive :: FilePath -> IO ZipArchive
readZipArchive f = B.readFile f >>= return . toZipArchive

-- | Writes a 'ZipArchive' structure to a zip file.
writeZipArchive :: FilePath -> ZipArchive -> IO ()
writeZipArchive f a = B.writeFile f (fromZipArchive a)

-- | Returns list of files in a zip archive.
filesInZipArchive :: ZipArchive -> [FilePath]
filesInZipArchive = (map eRelativePath) . zEntries

-- | Adds an entry to a zip archive, or updates an existing entry.
addEntryToZipArchive :: ZipEntry -> ZipArchive -> ZipArchive
addEntryToZipArchive entry archive =
  let archive' = deleteEntryFromZipArchive (eRelativePath entry) archive
      oldEntries = zEntries archive'
  in  archive' { zEntries = entry : oldEntries }

-- | Deletes an entry from a zip archive.
deleteEntryFromZipArchive :: FilePath -> ZipArchive -> ZipArchive
deleteEntryFromZipArchive path archive =
  let path' = zipifyFilePath path
      newEntries = filter (\e -> eRelativePath e /= path') $ zEntries archive
  in  archive { zEntries = newEntries }

-- | Returns the zip entry with the specified path, or Nothing.
findZipEntryByPath :: FilePath -> ZipArchive -> Maybe ZipEntry
findZipEntryByPath path archive = find (\e -> path == eRelativePath e) (zEntries archive)

-- | Returns uncompressed contents of zip entry.
contentsOfZipEntry :: ZipEntry -> Either String B.ByteString
contentsOfZipEntry entry =
  let uncompressedData = decompressData (eCompressionMethod entry) (eCompressedData entry)
  in  if eCRC32 entry == computeCRC32 uncompressedData
         then Right uncompressedData
         else Left "CRC32 mismatch"

-- | Generates a 'ZipEntry' from a file or directory.
readZipEntry :: FilePath -> IO ZipEntry
readZipEntry path = do
  isDir <- doesDirectoryExist path
  let path' = zipifyFilePath $ normalise $
              path ++ if isDir then "/" else ""  -- make sure directories end with /
  uncompressedData <- if isDir
                         then return B.empty
                         else B.readFile path
  let uncompressedSize = B.length uncompressedData
  let compressionMethod = if uncompressedSize < 30
                             then NoCompression
                             else Deflate
  let compressedData = compressData compressionMethod uncompressedData
  lastModDateTime <- getModificationTime path >>= return . clockTimeToMSDOSDateTime
  let crc32 = CRC32.calc_crc32 $ map w2c $ B.unpack uncompressedData
  return $ ZipEntry { eRelativePath            = path'
                    , eCompressionMethod       = compressionMethod
                    , eLastModified            = lastModDateTime
                    , eCRC32                   = crc32
                    , eCompressedSize          = fromIntegral $ B.length compressedData
                    , eUncompressedSize        = fromIntegral $ uncompressedSize
                    , eExtraField              = B.empty
                    , eFileComment             = B.empty
                    , eInternalFileAttributes  = 0  -- potentially non-text
                    , eExternalFileAttributes  = 0  -- appropriate if from stdin
                    , eCompressedData          = compressedData
                    }

-- | Writes contents of a 'ZipEntry' to a file.
writeZipEntry :: FilePath -> ZipEntry -> IO ()
writeZipEntry path entry = do
  case contentsOfZipEntry entry of
       Left e  -> error e
       Right c -> B.writeFile path c

-- | Uncompress a lazy bytestring.
compressData :: CompressionMethod -> B.ByteString -> B.ByteString
compressData Deflate       = Zlib.compress
compressData NoCompression = id

-- | Compress a lazy bytestring.
decompressData :: CompressionMethod -> B.ByteString -> B.ByteString
decompressData Deflate       = Zlib.decompress
decompressData NoCompression = id

-- | Add the specified files to a 'ZipArchive'.  If 'OptRecursive' is specified,
-- recursively add files contained in directories.  If 'OptVerbose' is specified,
-- print messages to stderr.
addFilesToZipArchive :: [ZipOptions] -> ZipArchive -> [FilePath] -> IO ZipArchive
addFilesToZipArchive opts archive files = do
  filesAndChildren <- if OptRecursive `elem` opts
                         then mapM getDirectoryContentsRecursive files >>= return . nub . concat
                         else return files
  let readZipEntry' f = do entry <- readZipEntry f
                           when (OptVerbose `elem` opts) $ do
                             let compmethod = case eCompressionMethod entry of
                                              Deflate       -> "deflated"
                                              NoCompression -> "stored"
                             hPutStrLn stderr $
                               printf "  adding: %s (%s %2.f%%)" (eRelativePath entry)
                               compmethod (100 - (100 * compressionRatio entry))
                           return entry
  entries <- mapM readZipEntry' filesAndChildren
  return $ foldr addEntryToZipArchive archive entries

-- | Extract all files from a 'ZipArchive', creating directories
-- as needed.  If 'OptVerbose' is specified, print messages to stderr.
-- Note that last modified times are supported only for POSIX, not for
-- Windows.
extractFilesFromZipArchive :: [ZipOptions] -> ZipArchive -> IO ()
extractFilesFromZipArchive opts archive = do
  let entries = zEntries archive
  let writeEntry' e = do let path = eRelativePath e
                         -- create directories
                         let dir = takeDirectory path
                         exists <- doesDirectoryExist dir
                         unless exists $ do
                           createDirectoryIfMissing True path
                           when (OptVerbose `elem` opts) $
                             hPutStrLn stderr $ "  creating: " ++ path
                         if length path > 0 && last path == '/' -- path is a directory
                            then return ()
                            else do
                              when (OptVerbose `elem` opts) $ do
                                hPutStrLn stderr $ case eCompressionMethod e of
                                                        Deflate       -> " inflating: " ++ path
                                                        NoCompression -> "extracting: " ++ path
                              writeZipEntry path e
                         setFileTimeStamp path (eLastModified e)
  mapM_ writeEntry' entries

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

-- | Convert a clock time to a MSDOS datetime.  The MSDOS time will be relative to UTC.
clockTimeToMSDOSDateTime :: ClockTime -> MSDOSDateTime
clockTimeToMSDOSDateTime clocktime =
  let ut = toUTCTime clocktime
      dosTime = toEnum $ (ctSec ut `div` 2) + shiftL (ctMin ut) 5 + shiftL (ctHour ut) 11
      dosDate = toEnum $ ctDay ut + shiftL (fromEnum (ctMonth ut) + 1) 5 + shiftL (ctYear ut - 1980) 9
  in  MSDOSDateTime { msDOSDate = dosDate, msDOSTime = dosTime }

-- | Convert a MSDOS datetime to a 'ClockTime'.
msDOSDateTimeToClockTime :: MSDOSDateTime -> ClockTime
msDOSDateTimeToClockTime (MSDOSDateTime {msDOSDate = dosDate, msDOSTime = dosTime}) =
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
  in  addToClockTime timeSinceEpoch (toClockTime epoch)

-- | The beginning of the unix epoch.
epoch :: CalendarTime
epoch = CalendarTime { ctYear = 1970, ctMonth = January, ctDay = 1,
                       ctHour = 0, ctMin = 0, ctSec = 0,
                       ctPicosec = 0, ctWDay = Thursday, ctYDay = 0,
                       ctTZName = "UTC", ctTZ = 0, ctIsDST = False}

--------------------------------------------------------------------------------
-- Internal functions for reading and writing zip binary format.

-- | Perform a sequence of actions until one returns Nothing;
-- return list of results.
many :: Monad m => m (Maybe a) -> m [a]
many p = do
  r <- p
  case r of
       Just x  ->  many p >>= return . (x:)
       Nothing -> return []

-- Note that even on Windows, zip files use "/" internally as path separator.
zipifyFilePath :: FilePath -> String
zipifyFilePath path =
  let dir = takeDirectory path
      fn  = takeFileName path
      dirParts = splitDirectories dir
  in  concat (map (++ "/") dirParts) ++ fn

computeCRC32 :: B.ByteString -> Word32
computeCRC32 = CRC32.calc_crc32 . map w2c . B.unpack

compressionRatio :: ZipEntry -> Float
compressionRatio entry =
  if eUncompressedSize entry == 0
     then 1
     else fromIntegral (eCompressedSize entry) / fromIntegral (eUncompressedSize entry)

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       contents <- getDirectoryContents path
       let contents' = map (path </>) $ filter (`notElem` ["..","."]) contents
       children <- mapM getDirectoryContentsRecursive contents'
       return (path : concat children)
     else return [path]

setFileTimeStamp :: FilePath -> MSDOSDateTime -> IO ()
setFileTimeStamp file datetime = do
#ifdef _WINDOWS
  return ()  -- TODO - figure out how to set the timestamp on Windows
#else
  let (TOD epochtime _) = msDOSDateTimeToClockTime datetime
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

getZipArchive :: Get ZipArchive
getZipArchive = do
  locals <- many getLocalFile
  files <- many (getFileHeader locals)
  digSig <- lookAheadM getDigitalSignature
  endSig <- getWord32le
  unless (endSig == 0x06054b50) $ fail "Did not find end of central directory signature"
  skip 2 -- disk number
  skip 2 -- disk number of central directory
  skip 2 -- num entries on this disk
  skip 2 -- num entries in central directory
  skip 4 -- central directory size
  skip 4 -- offset of central directory
  commentLength <- getWord16le
  zipComment <- getLazyByteString (toEnum $ fromEnum commentLength)
  return $ ZipArchive
           { zEntries                = files
           , zSignature              = digSig
           , zComment                = zipComment
           }

putZipArchive :: ZipArchive -> Put
putZipArchive archive = do
  mapM_ putLocalFile $ zEntries archive
  let localFileSizes = map localFileSize $ zEntries archive
  let offsets = scanl (+) 0 localFileSizes
  let cdOffset = last offsets
  zipWithM putFileHeader offsets (zEntries archive)
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


fileHeaderSize :: ZipEntry -> Word32
fileHeaderSize f =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 +
    fromIntegral (B.length $ fromString $ zipifyFilePath $ eRelativePath f) +
    B.length (eExtraField f) + B.length (eFileComment f)

localFileSize :: ZipEntry -> Word32
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

getLocalFile :: Get (Maybe (Word32, B.ByteString))
getLocalFile = do
  sig <- lookAhead getWord32le
  if sig /= 0x04034b50
    then return Nothing
    else do
      offset <- bytesRead
      skip 4  -- signature
      skip 2  -- version
      skip 2  -- general purpose bit flag
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
      compressedData <- getLazyByteString (fromIntegral compressedSize)
      return $ Just (fromIntegral offset, compressedData)

putLocalFile :: ZipEntry -> Put
putLocalFile f = do
  putWord32le 0x04034b50
  putWord16le 20 -- version needed to extract (>=2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
  putWord16le $ case eCompressionMethod f of
                     NoCompression -> 0
                     Deflate       -> 8
  putWord16le $ msDOSTime $ eLastModified f
  putWord16le $ msDOSDate $ eLastModified f
  putWord32le $ eCRC32 f
  putWord32le $ eCompressedSize f
  putWord32le $ eUncompressedSize f
  putWord16le $ fromIntegral $ length $ eRelativePath f
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

getFileHeader :: [(Word32, B.ByteString)]  -- ^ list of (offset, content) pairs returned by getLocalFile
              -> Get (Maybe ZipEntry)
getFileHeader locals = do
  sig <- lookAhead getWord32le
  if sig /= 0x02014b50
     then return Nothing
     else do
       skip 4 -- skip past signature
       skip 2 -- version made by
       versionNeededToExtract <- getWord16le
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
       compressedData <- case lookup relativeOffset locals of
                         Just x  -> return x
                         Nothing -> fail $ "Unable to find data at offset " ++ show relativeOffset
       return $ Just $ ZipEntry
                 { eRelativePath            = toString fileName
                 , eCompressionMethod       = compressionMethod
                 , eLastModified            = MSDOSDateTime { msDOSDate = lastModFileDate,
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
              -> ZipEntry
              -> Put
putFileHeader offset local = do
  putWord32le 0x02014b50
  putWord16le 0  -- version made by
  putWord16le 20 -- version needed to extract (>= 2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
  putWord16le $ case eCompressionMethod local of
                     NoCompression -> 0
                     Deflate       -> 8
  putWord16le $ msDOSTime $ eLastModified local
  putWord16le $ msDOSDate $ eLastModified local
  putWord32le $ eCRC32 local
  putWord32le $ eCompressedSize local
  putWord32le $ eUncompressedSize local
  putWord16le $ fromIntegral $ length $ eRelativePath local
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

getDigitalSignature :: Get (Maybe B.ByteString)
getDigitalSignature = do
  hdrSig <- getWord32le
  if hdrSig /= 0x08064b50
     then return Nothing
     else do
        sigSize <- getWord16le
        getLazyByteString (toEnum $ fromEnum sigSize) >>= return . Just

putDigitalSignature :: Maybe B.ByteString -> Put
putDigitalSignature Nothing = return ()
putDigitalSignature (Just sig) = do
  putWord32le 0x08064b50
  putWord16le $ fromIntegral $ B.length sig
  putLazyByteString sig

