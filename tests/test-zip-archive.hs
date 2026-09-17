{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Test suite for Codec.Archive.Zip
-- runghc Test.hs

import Codec.Archive.Zip
import Control.Monad (unless)
import Data.Bits
import Data.Word (Word8)
import Control.Exception (try, catch, SomeException)
import System.Directory hiding (isSymbolicLink)
import Test.HUnit.Base
import Test.HUnit.Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Exit
import System.IO.Temp (withTempDirectory)

#ifndef _WINDOWS
import System.FilePath.Posix
import System.Posix.Files
import System.Process (rawSystem)
#else
import System.FilePath.Windows
#endif

-- define equality for Archives so timestamps aren't distinguished if they
-- correspond to the same MSDOS datetime.
-- build a minimal raw zip archive containing a single stored entry
-- with empty contents (CRC32 = 0), a given general purpose bit flag,
-- and the given raw file name bytes
mkRawZip :: Int -> [Word8] -> BL.ByteString
mkRawZip flag name = BL.pack (local ++ central ++ eocd)
 where
  n = length name
  le16, le32 :: Int -> [Word8]
  le16 x = [fromIntegral (x .&. 0xff), fromIntegral ((x `shiftR` 8) .&. 0xff)]
  le32 x = le16 (x .&. 0xffff) ++ le16 ((x `shiftR` 16) .&. 0xffff)
  local = [0x50,0x4b,0x03,0x04] ++ le16 20 ++ le16 flag ++ le16 0 -- stored
          ++ le16 0 ++ le16 0x21          -- mod time/date (1980-01-01)
          ++ le32 0 ++ le32 0 ++ le32 0   -- crc, csize, usize
          ++ le16 n ++ le16 0 ++ name
  central = [0x50,0x4b,0x01,0x02] ++ le16 20 ++ le16 20 ++ le16 flag
          ++ le16 0 ++ le16 0 ++ le16 0x21
          ++ le32 0 ++ le32 0 ++ le32 0
          ++ le16 n ++ le16 0 ++ le16 0   -- name/extra/comment len
          ++ le16 0 ++ le16 0 ++ le32 0   -- disk, int attrs, ext attrs
          ++ le32 0                       -- local header offset
          ++ name
  eocd = [0x50,0x4b,0x05,0x06] ++ le16 0 ++ le16 0 ++ le16 1 ++ le16 1
          ++ le32 (46 + n) ++ le32 (30 + n) ++ le16 0

instance Eq Archive where
  (==) a1 a2 =  zSignature a1 == zSignature a2
             && zComment a1 == zComment a2
             && (all id $ zipWith (\x y -> x { eLastModified = eLastModified x `div` 2  } ==
                                           y { eLastModified = eLastModified y `div` 2  }) (zEntries a1) (zEntries a2))

#ifndef _WINDOWS

-- construct an Entry that represents a symbolic link, as found in
-- archives produced by Info-ZIP and this library
mkSymlinkEntry :: FilePath -> String -> Entry
mkSymlinkEntry path target =
  (toEntry path 0 (BLC.pack target))
    { eRelativePath = path
    , eVersionMadeBy = 0x0300 -- UNIX
    , eExternalFileAttributes =
        fromIntegral (shiftL (fromIntegral symbolicLinkMode .|. (0o777 :: Integer)) 16)
    }

createTestDirectoryWithSymlinks :: FilePath -> FilePath -> IO FilePath
createTestDirectoryWithSymlinks prefixDir  baseDir = do
  let testDir = prefixDir </> baseDir
  createDirectoryIfMissing True testDir
  createDirectoryIfMissing True (testDir </> "1")
  writeFile (testDir </> "1/file.txt") "hello"
  cwd <- getCurrentDirectory
  createSymbolicLink (cwd </> testDir </> "1/file.txt") (testDir </> "link_to_file")
  createSymbolicLink (cwd </> testDir </> "1") (testDir </> "link_to_directory")
  return testDir

#endif



main :: IO Counts
main = withTempDirectory "." "test-zip-archive." $ \tmpDir -> do
#ifndef _WINDOWS
  ec <- catch (rawSystem "command" ["-v", "unzip"])
         (\(_ :: SomeException) -> rawSystem "which" ["unzip"])
  let unzipInPath = ec == ExitSuccess
  unless unzipInPath $
    putStrLn "\n\nunzip is not in path; skipping testArchiveAndUnzip\n"
#endif
  res   <- runTestTT $ TestList $ map (\f -> f tmpDir) $
                                [ testReadWriteArchive
                                , testReadExternalZip
                                , testFromToArchive
                                , testReadWriteEntry
                                , testAddFilesOptions
                                , testDeleteEntries
                                , testExtractFiles
                                , testExtractFilesFailOnEncrypted
                                , testPasswordProtectedRead
                                , testIncorrectPasswordRead
                                , testEvilPath
                                , testAbsolutePath
                                , testFileNameEncodings
#ifndef _WINDOWS
                                , testExtractFilesWithPosixAttrs
                                , testArchiveExtractSymlinks
                                , testExtractExternalZipWithSymlinks
                                , testExtractOverwriteExternalZipWithSymlinks
                                , testEvilSymlinkPath
                                , testEvilSymlinkChain
#endif
                                ]
#ifndef _WINDOWS
                                ++ [testArchiveAndUnzip | unzipInPath]
#endif
  exitWith $ case (failures res + errors res) of
                     0 -> ExitSuccess
                     n -> ExitFailure n

testReadWriteArchive :: FilePath -> Test
testReadWriteArchive tmpDir = TestCase $ do
  archive <- addFilesToArchive [OptRecursive] emptyArchive ["LICENSE", "src"]
  BL.writeFile (tmpDir </> "test1.zip") $ fromArchive archive
  archive' <- toArchive <$> BL.readFile (tmpDir </> "test1.zip")
  assertEqual "for writing and reading test1.zip" archive archive'
  assertEqual "for writing and reading test1.zip" archive archive'

testReadExternalZip :: FilePath -> Test
testReadExternalZip _tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/test4.zip"
  let files = filesInArchive archive
  assertEqual "for results of filesInArchive"
    ["test4/","test4/a.txt","test4/b.bin","test4/c/",
     "test4/c/with spaces.txt"] files
  bContents <- BL.readFile "tests/test4/b.bin"
  case findEntryByPath "test4/b.bin" archive of
       Nothing  -> assertFailure "test4/b.bin not found in archive"
       Just f   -> do
                    assertEqual "for text4/b.bin file entry"
                      NoEncryption (eEncryptionMethod f)
                    assertEqual "for contents of test4/b.bin in archive"
                      bContents (fromEntry f)
  case findEntryByPath "test4/" archive of
       Nothing  -> assertFailure "test4/ not found in archive"
       Just f   -> assertEqual "for contents of test4/ in archive"
                      BL.empty (fromEntry f)

testFromToArchive :: FilePath -> Test
testFromToArchive tmpDir = TestCase $ do
  archive1 <- addFilesToArchive [OptRecursive] emptyArchive ["LICENSE", "src"]
  assertEqual "for (toArchive $ fromArchive archive)" archive1 (toArchive $ fromArchive archive1)
#ifndef _WINDOWS
  testDir <- createTestDirectoryWithSymlinks tmpDir "test_dir_with_symlinks"
  archive2 <- addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] emptyArchive [testDir]
  assertEqual "for (toArchive $ fromArchive archive)" archive2 (toArchive $ fromArchive archive2)
#endif

testReadWriteEntry :: FilePath -> Test
testReadWriteEntry tmpDir = TestCase $ do
  entry <- readEntry [] "zip-archive.cabal"
  setCurrentDirectory tmpDir
  writeEntry [] entry
  setCurrentDirectory ".."
  entry' <- readEntry [] (tmpDir </> "zip-archive.cabal")
  let entry'' = entry' { eRelativePath = eRelativePath entry, eLastModified = eLastModified entry }
  assertEqual "for readEntry -> writeEntry -> readEntry" entry entry''

testAddFilesOptions :: FilePath -> Test
testAddFilesOptions tmpDir = TestCase $ do
  archive1 <- addFilesToArchive [OptVerbose] emptyArchive ["LICENSE", "src"]
  archive2 <- addFilesToArchive [OptRecursive, OptVerbose] archive1 ["LICENSE", "src"]
  assertBool "for recursive and nonrecursive addFilesToArchive"
     (length (filesInArchive archive1) < length (filesInArchive archive2))
#ifndef _WINDOWS
  testDir <- createTestDirectoryWithSymlinks tmpDir "test_dir_with_symlinks2"
  archive3 <- addFilesToArchive [OptVerbose, OptRecursive] emptyArchive [testDir]
  archive4 <- addFilesToArchive [OptVerbose, OptRecursive, OptPreserveSymbolicLinks] emptyArchive [testDir]
  mapM_ putStrLn $ filesInArchive archive3
  mapM_ putStrLn $ filesInArchive archive4
  assertBool "for recursive and recursive by preserving symlinks addFilesToArchive"
     (length (filesInArchive archive4) < length (filesInArchive archive3))
#endif


testDeleteEntries :: FilePath -> Test
testDeleteEntries _tmpDir = TestCase $ do
  archive1 <- addFilesToArchive [] emptyArchive ["LICENSE", "src"]
  let archive2 = deleteEntryFromArchive "LICENSE" archive1
  let archive3 = deleteEntryFromArchive "src" archive2
  assertEqual "for deleteFilesFromArchive" emptyArchive archive3

testFileNameEncodings :: FilePath -> Test
testFileNameEncodings _tmpDir = TestCase $ do
  -- bit 11 clear: name is in IBM code page 437 (0x82 = 'é')
  case toArchiveOrFail (mkRawZip 0 [0x82]) of
    Left err -> assertFailure $ "could not parse CP437 archive: " ++ err
    Right a  -> assertEqual "for CP437 file name" ["\233"] (filesInArchive a)
  -- bit 11 set: name is UTF-8 ('é' = 0xC3 0xA9)
  case toArchiveOrFail (mkRawZip 0x800 [0xc3, 0xa9]) of
    Left err -> assertFailure $ "could not parse UTF-8 archive: " ++ err
    Right a  -> assertEqual "for UTF-8 file name" ["\233"] (filesInArchive a)
  -- bit 11 set but name is invalid UTF-8: decode leniently, don't crash
  result <- try $ case toArchiveOrFail (mkRawZip 0x800 [0x82]) of
                    Left err -> return [err]
                    Right a  -> mapM (\f -> length f `seq` return f)
                                     (filesInArchive a)
              :: IO (Either SomeException [FilePath])
  case result of
    Left err -> assertFailure $ "invalid UTF-8 name raised: " ++ show err
    Right fs -> assertEqual "for invalid UTF-8 file name" ["\65533"] fs

testAbsolutePath :: FilePath -> Test
testAbsolutePath tmpDir = TestCase $ do
  -- an entry with an absolute path must not escape OptDestination
  -- (note that dest </> "/absolute/evil" == "/absolute/evil")
  let entry = (toEntry "placeholder" 0 (BLC.pack "boom"))
                { eRelativePath = "/absolute/evil" }
  result <- try $ writeEntry [OptDestination (tmpDir </> "absdest")] entry
              :: IO (Either ZipException ())
  case result of
    Left err -> assertEqual "exception for absolute path"
                  (UnsafePath "/absolute/evil") err
    Right _  -> assertFailure "writeEntry should have failed on absolute path"

testEvilPath :: FilePath -> Test
testEvilPath _tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/zip_with_evil_path.zip"
  result <- try $ extractFilesFromArchive [] archive :: IO (Either ZipException ())
  case result of
    Left err -> assertBool "Wrong exception" $ err == UnsafePath "../evil"
    Right _ -> assertFailure "extractFilesFromArchive should have failed"

testExtractFiles :: FilePath -> Test
testExtractFiles tmpDir = TestCase $ do
  createDirectory (tmpDir </> "dir1")
  createDirectory (tmpDir </> "dir1/dir2")
  let hiMsg = BS.pack "hello there"
  let helloMsg = BS.pack "Hello there. This file is very long.  Longer than 31 characters."
  BS.writeFile (tmpDir </> "dir1/hi") hiMsg
  BS.writeFile (tmpDir </> "dir1/dir2/hello") helloMsg
  archive <- addFilesToArchive [OptRecursive] emptyArchive [(tmpDir </> "dir1")]
  removeDirectoryRecursive (tmpDir </> "dir1")
  extractFilesFromArchive [OptVerbose] archive
  hi <- BS.readFile (tmpDir </> "dir1/hi")
  hello <- BS.readFile (tmpDir </> "dir1/dir2/hello")
  assertEqual ("contents of " </> tmpDir </> "dir1/hi") hiMsg hi
  assertEqual ("contents of " </> tmpDir </> "dir1/dir2/hello") helloMsg hello

testExtractFilesFailOnEncrypted :: FilePath -> Test
testExtractFilesFailOnEncrypted tmpDir = TestCase $ do
  let dir = tmpDir </> "fail-encrypted"
  createDirectory dir

  archive <- toArchive <$> BL.readFile "tests/zip_with_password.zip"
  result <- try $ extractFilesFromArchive [OptDestination dir] archive :: IO (Either ZipException ())
  removeDirectoryRecursive dir

  case result of
    Left err -> assertBool "Wrong exception" $ err == CannotWriteEncryptedEntry "test.txt"
    Right _ -> assertFailure "extractFilesFromArchive should have failed"

testPasswordProtectedRead :: FilePath -> Test
testPasswordProtectedRead _tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/zip_with_password.zip"

  assertEqual "for results of filesInArchive" ["test.txt"] (filesInArchive archive)
  case findEntryByPath "test.txt" archive of
       Nothing  -> assertFailure "test.txt not found in archive"
       Just f   -> do
            assertBool "for encrypted test.txt file entry"
              (isEncryptedEntry f)
            assertEqual "for contents of test.txt in archive"
              (Just $ BLC.pack "SUCCESS\n") (fromEncryptedEntry "s3cr3t" f)

testIncorrectPasswordRead :: FilePath -> Test
testIncorrectPasswordRead _tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/zip_with_password.zip"
  case findEntryByPath "test.txt" archive of
       Nothing  -> assertFailure "test.txt not found in archive"
       Just f   -> do
            assertEqual "for contents of test.txt in archive"
              Nothing (fromEncryptedEntry "INCORRECT" f)

#ifndef _WINDOWS

testExtractFilesWithPosixAttrs :: FilePath -> Test
testExtractFilesWithPosixAttrs tmpDir = TestCase $ do
  createDirectory (tmpDir </> "dir3")
  let hiMsg = "hello there"
  writeFile (tmpDir </> "dir3/hi") hiMsg
  let perms = unionFileModes ownerReadMode $ unionFileModes ownerWriteMode ownerExecuteMode
  setFileMode (tmpDir </> "dir3/hi") perms
  archive <- addFilesToArchive [OptRecursive] emptyArchive [(tmpDir </> "dir3")]
  removeDirectoryRecursive (tmpDir </> "dir3")
  extractFilesFromArchive [OptVerbose] archive
  hi <- readFile (tmpDir </> "dir3/hi")
  fm <- fmap fileMode $ getFileStatus (tmpDir </> "dir3/hi")
  assertEqual "file modes" perms (intersectFileModes perms fm)
  assertEqual ("contents of " </> tmpDir </> "dir3/hi") hiMsg hi

testArchiveExtractSymlinks :: FilePath -> Test
testArchiveExtractSymlinks tmpDir = TestCase $ do
  testDir <- createTestDirectoryWithSymlinks tmpDir "test_dir_with_symlinks3"
  let locationDir = "location_dir"
  archive <- addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks, OptLocation locationDir True] emptyArchive [testDir]
  removeDirectoryRecursive testDir
  let destination = "test_dest"
  extractFilesFromArchive [OptPreserveSymbolicLinks, OptDestination destination] archive
  isDirSymlink <- pathIsSymbolicLink (destination </> locationDir </> testDir </> "link_to_directory")
  isFileSymlink <- pathIsSymbolicLink (destination </> locationDir </> testDir </> "link_to_file")
  assertBool "Symbolic link to directory is preserved" isDirSymlink
  assertBool "Symbolic link to file is preserved" isFileSymlink
  removeDirectoryRecursive destination

testExtractExternalZipWithSymlinks :: FilePath -> Test
testExtractExternalZipWithSymlinks tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/zip_with_symlinks.zip"
  extractFilesFromArchive [OptPreserveSymbolicLinks, OptDestination tmpDir] archive
  let zipRootDir = "zip_test_dir_with_symlinks"
      symlinkDir = tmpDir </> zipRootDir </> "symlink_to_dir_1"
      symlinkFile = tmpDir </> zipRootDir </> "symlink_to_file_1"
  isDirSymlink <- pathIsSymbolicLink symlinkDir
  targetDirExists <- doesDirectoryExist symlinkDir
  isFileSymlink <- pathIsSymbolicLink symlinkFile
  targetFileExists <- doesFileExist symlinkFile
  assertBool "Symbolic link to directory is preserved" isDirSymlink
  assertBool "Target directory exists" targetDirExists
  assertBool "Symbolic link to file is preserved" isFileSymlink
  assertBool "Target file exists" targetFileExists
  removeDirectoryRecursive tmpDir

testExtractOverwriteExternalZipWithSymlinks :: FilePath -> Test
testExtractOverwriteExternalZipWithSymlinks tmpDir = TestCase $ do
  archive <- toArchive <$> BL.readFile "tests/zip_with_symlinks.zip"
  extractFilesFromArchive [OptPreserveSymbolicLinks, OptDestination tmpDir] archive
  asserts
  extractFilesFromArchive [OptPreserveSymbolicLinks, OptDestination tmpDir] archive
  asserts
  where
    zipRootDir = "zip_test_dir_with_symlinks"
    symlinkDir = tmpDir </> zipRootDir </> "symlink_to_dir_1"
    symlinkFile = tmpDir </> zipRootDir </> "symlink_to_file_1"
    asserts = do
      isDirSymlink <- pathIsSymbolicLink symlinkDir
      targetDirExists <- doesDirectoryExist symlinkDir
      isFileSymlink <- pathIsSymbolicLink symlinkFile
      targetFileExists <- doesFileExist symlinkFile
      assertBool "Symbolic link to directory is preserved" isDirSymlink
      assertBool "Target directory exists" targetDirExists
      assertBool "Symbolic link to file is preserved" isFileSymlink
      assertBool "Target file exists" targetFileExists

testEvilSymlinkPath :: FilePath -> Test
testEvilSymlinkPath tmpDir = TestCase $ do
  let dest = tmpDir </> "symlink-dest1"
  createDirectoryIfMissing True dest
  let entry = mkSymlinkEntry "../evil-link" "/tmp"
  result <- try $ writeSymbolicLinkEntry
                    [OptPreserveSymbolicLinks, OptDestination dest] entry
              :: IO (Either ZipException ())
  case result of
    Left err -> assertEqual "exception for evil symlink path"
                  (UnsafePath "../evil-link") err
    Right _  -> assertFailure "writeSymbolicLinkEntry should have failed"
  evilExists <- pathIsSymbolicLink (tmpDir </> "evil-link")
                  `catch` (\(_ :: SomeException) -> return False)
  assertBool "no symlink was created outside the destination" (not evilExists)

testEvilSymlinkChain :: FilePath -> Test
testEvilSymlinkChain tmpDir = TestCase $ do
  let dest = tmpDir </> "symlink-dest2"
  let outside = tmpDir </> "outside"
  createDirectoryIfMissing True dest
  createDirectoryIfMissing True outside
  cwd <- getCurrentDirectory
  -- first entry creates a symlink pointing outside the destination;
  -- second entry tries to create a symlink through it
  let archive = Archive [ mkSymlinkEntry "sub" (cwd </> outside)
                        , mkSymlinkEntry "sub/inner" "anywhere"
                        ] Nothing BL.empty
  result <- try $ extractFilesFromArchive
                    [OptPreserveSymbolicLinks, OptDestination dest] archive
              :: IO (Either ZipException ())
  case result of
    Left err -> assertEqual "exception for chained symlink"
                  (UnsafePath "sub/inner") err
    Right _  -> assertFailure "extractFilesFromArchive should have failed"
  innerExists <- pathIsSymbolicLink (outside </> "inner")
                  `catch` (\(_ :: SomeException) -> return False)
  assertBool "no symlink was created through another symlink" (not innerExists)

testArchiveAndUnzip :: FilePath -> Test
testArchiveAndUnzip tmpDir = TestCase $ do
  let dir = "test_dir_with_symlinks4"
  testDir <- createTestDirectoryWithSymlinks tmpDir dir
  archive <- addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] emptyArchive [testDir]
  removeDirectoryRecursive testDir
  let zipFile = tmpDir </> "testUnzip.zip"
  BL.writeFile zipFile $ fromArchive archive
  ec <- rawSystem "unzip" [zipFile]
  assertBool "unzip succeeds" $ ec == ExitSuccess
  let symlinkDir = testDir </> "link_to_directory"
      symlinkFile = testDir </> "link_to_file"
  isDirSymlink <- pathIsSymbolicLink symlinkDir
  targetDirExists <- doesDirectoryExist symlinkDir
  isFileSymlink <- pathIsSymbolicLink symlinkFile
  targetFileExists <- doesFileExist symlinkFile
  assertBool "Symbolic link to directory is preserved" isDirSymlink
  assertBool "Target directory exists" targetDirExists
  assertBool "Symbolic link to file is preserved" isFileSymlink
  assertBool "Target file exists" targetFileExists
  removeDirectoryRecursive tmpDir

#endif
