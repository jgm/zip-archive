{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- Test suite for Codec.Archive.Zip
-- runghc Test.hs

import Codec.Archive.Zip
import System.Directory
import Test.HUnit.Base
import Test.HUnit.Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import System.Exit
import System.IO.Temp (withTempDirectory)

#ifndef _WINDOWS
import System.Posix.Files
#endif

-- define equality for Archives so timestamps aren't distinguished if they
-- correspond to the same MSDOS datetime.
instance Eq Archive where
  (==) a1 a2 =  zSignature a1 == zSignature a2
             && zComment a1 == zComment a2
             && (all id $ zipWith (\x y -> x { eLastModified = eLastModified x `div` 2  } ==
                                           y { eLastModified = eLastModified y `div` 2  }) (zEntries a1) (zEntries a2))

main :: IO Counts
main = withTempDirectory "." "test-zip-archive." $ \tmpDir -> do
  res   <- runTestTT $ TestList $ map (\f -> f tmpDir)
                                [ testReadWriteArchive
                                , testReadExternalZip
                                , testFromToArchive
                                , testReadWriteEntry
                                , testAddFilesOptions
                                , testDeleteEntries
                                , testExtractFiles
#ifndef _WINDOWS
                                , testExtractFilesWithPosixAttrs
#endif
                                ]
  exitWith $ case (failures res + errors res) of
                     0 -> ExitSuccess
                     n -> ExitFailure n

testReadWriteArchive :: FilePath -> Test
testReadWriteArchive tmpDir = TestCase $ do
  archive <- addFilesToArchive [OptRecursive] emptyArchive ["LICENSE", "src"]
  BL.writeFile (tmpDir ++ "/test1.zip") $ fromArchive archive
  archive' <- toArchive <$> BL.readFile (tmpDir ++ "/test1.zip")
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
       Just f   -> assertEqual "for contents of test4/b.bin in archive"
                      bContents (fromEntry f)
  case findEntryByPath "test4/" archive of
       Nothing  -> assertFailure "test4/ not found in archive"
       Just f   -> assertEqual "for contents of test4/ in archive"
                      BL.empty (fromEntry f)

testFromToArchive :: FilePath -> Test
testFromToArchive tmpDir = TestCase $ do
  archive1 <- addFilesToArchive [OptRecursive] emptyArchive ["LICENSE", "src"]
  assertEqual "for (toArchive $ fromArchive archive)" archive1 (toArchive $ fromArchive archive1)
  let testdir = tmpDir ++ "/test_dir_with_symlinks"
  createDirectory testdir
  createDirectory (testdir ++ "/1")
  writeFile (testdir ++ "/1/file.txt") "hello"
  createFileLink (testdir ++ "/1/file.txt") (testdir ++ "/link_to_file")
  createDirectoryLink (testdir ++ "/1") (testdir ++ "/link_to_directory")
  archive2 <- addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] emptyArchive [tmpDir ++ "/test_dir_with_symlinks"]
  assertEqual "for (toArchive $ fromArchive archive)" archive2 (toArchive $ fromArchive archive2)

testReadWriteEntry :: FilePath -> Test
testReadWriteEntry tmpDir = TestCase $ do
  entry <- readEntry [] "zip-archive.cabal"
  setCurrentDirectory tmpDir
  writeEntry [] entry
  setCurrentDirectory ".."
  entry' <- readEntry [] (tmpDir ++ "/zip-archive.cabal")
  let entry'' = entry' { eRelativePath = eRelativePath entry, eLastModified = eLastModified entry }
  assertEqual "for readEntry -> writeEntry -> readEntry" entry entry''

testAddFilesOptions :: FilePath -> Test
testAddFilesOptions tmpDir = TestCase $ do
  archive1 <- addFilesToArchive [OptVerbose] emptyArchive ["LICENSE", "src"]
  archive2 <- addFilesToArchive [OptRecursive, OptVerbose] archive1 ["LICENSE", "src"]
  assertBool "for recursive and nonrecursive addFilesToArchive"
     (length (filesInArchive archive1) < length (filesInArchive archive2))
#ifndef _WINDOWS
  let testdir = tmpDir ++ "/test_dir_with_symlinks2"
  createDirectory testdir
  createDirectory (testdir ++ "/1")
  writeFile (testdir ++ "/1/file.txt") "hello"
  createFileLink (testdir ++ "/1/file.txt") (testdir ++ "/link_to_file")
  createDirectoryLink (testdir ++ "/1") (testdir ++ "/link_to_directory")
  archive3 <- addFilesToArchive [OptVerbose, OptRecursive] emptyArchive [testdir]
  archive4 <- addFilesToArchive [OptVerbose, OptRecursive, OptPreserveSymbolicLinks] emptyArchive [testdir]
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

testExtractFiles :: FilePath -> Test
testExtractFiles tmpDir = TestCase $ do
  createDirectory (tmpDir ++ "/dir1")
  createDirectory (tmpDir ++ "/dir1/dir2")
  let hiMsg = BS.pack "hello there"
  let helloMsg = BS.pack "Hello there. This file is very long.  Longer than 31 characters."
  BS.writeFile (tmpDir ++ "/dir1/hi") hiMsg
  BS.writeFile (tmpDir ++ "/dir1/dir2/hello") helloMsg
  archive <- addFilesToArchive [OptRecursive] emptyArchive [(tmpDir ++ "/dir1")]
  removeDirectoryRecursive (tmpDir ++ "/dir1")
  extractFilesFromArchive [OptVerbose] archive
  hi <- BS.readFile (tmpDir ++ "/dir1/hi")
  hello <- BS.readFile (tmpDir ++ "/dir1/dir2/hello")
  assertEqual ("contents of " ++ tmpDir ++ "/dir1/hi") hiMsg hi
  assertEqual ("contents of " ++ tmpDir ++ "/dir1/dir2/hello") helloMsg hello

#ifndef _WINDOWS
testExtractFilesWithPosixAttrs :: FilePath -> Test
testExtractFilesWithPosixAttrs tmpDir = TestCase $ do
  createDirectory (tmpDir ++ "/dir3")
  let hiMsg = "hello there"
  writeFile (tmpDir ++ "/dir3/hi") hiMsg
  let perms = unionFileModes ownerReadMode $ unionFileModes ownerWriteMode ownerExecuteMode
  setFileMode (tmpDir ++ "/dir3/hi") perms
  archive <- addFilesToArchive [OptRecursive] emptyArchive [(tmpDir ++ "/dir3")]
  removeDirectoryRecursive (tmpDir ++ "/dir3")
  extractFilesFromArchive [OptVerbose] archive
  hi <- readFile (tmpDir ++ "/dir3/hi")
  fm <- fmap fileMode $ getFileStatus (tmpDir ++ "/dir3/hi")
  assertEqual "file modes" perms (intersectFileModes perms fm)
  assertEqual ("contents of " ++ tmpDir ++ "/dir3/hi") hiMsg hi
#endif
