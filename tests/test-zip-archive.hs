{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- Test suite for Codec.Archive.Zip
-- runghc Test.hs

import Codec.Archive.Zip
import Control.Applicative
import Control.Monad (unless)
import Control.Exception (try)
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
instance Eq Archive where
  (==) a1 a2 =  zSignature a1 == zSignature a2
             && zComment a1 == zComment a2
             && (all id $ zipWith (\x y -> x { eLastModified = eLastModified x `div` 2  } ==
                                           y { eLastModified = eLastModified y `div` 2  }) (zEntries a1) (zEntries a2))

#ifndef _WINDOWS

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
  ec <- rawSystem "which" ["unzip"]
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
#ifndef _WINDOWS
                                , testExtractFilesWithPosixAttrs
                                , testArchiveExtractSymlinks
                                , testExtractExternalZipWithSymlinks
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
