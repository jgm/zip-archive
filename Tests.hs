-- Test suite for Codec.Archive.Zip
-- runghc Test.hs

import Codec.Archive.Zip
import System.Directory
import Test.HUnit.Base
import Test.HUnit.Text
import System.Time
import System.Process
import qualified Data.ByteString.Lazy as B

-- define equality for ZipArchives so timestamps aren't distinguished if they
-- correspond to the same MSDOS datetime.
instance Eq ZipArchive where
  (==) a1 a2 =  zSignature a1 == zSignature a2
             && zComment a1 == zComment a2
             && (all id $ zipWith (\x y -> x { eLastModified = eLastModified x `div` 2  } ==
                                           y { eLastModified = eLastModified y `div` 2  }) (zEntries a1) (zEntries a2))

main :: IO Counts
main = do
  createDirectory "test-temp"
  counts <- runTestTT $ TestList [ 
                                   testReadWriteArchive
                                 , testReadExternalZip
                                 , testFromToZipArchive
                                 , testReadWriteZipEntry
                                 , testAddFilesOptions
                                 , testDeleteEntries
                                 , testExtractFiles
                                 ]
  removeDirectoryRecursive "test-temp"
  return counts

testReadWriteArchive = TestCase $ do
  archive <- addFilesToZipArchive [OptRecursive] emptyZipArchive ["LICENSE", "Codec"]
  writeZipArchive "test-temp/test1.zip" archive
  archive' <- readZipArchive "test-temp/test1.zip"
  assertEqual "for writing and reading test1.zip" archive archive'
  assertEqual "for writing and reading test1.zip" archive archive'

testReadExternalZip = TestCase $ do
  runCommand "zip -q -9 test-temp/test4.zip zip-archive.cabal Codec/Archive/Zip.hs" >>= waitForProcess
  archive <- readZipArchive "test-temp/test4.zip"
  let files = filesInZipArchive archive
  assertEqual "for results of filesInZipArchive" ["zip-archive.cabal", "Codec/Archive/Zip.hs"] files
  cabalContents <- B.readFile "zip-archive.cabal"
  case findZipEntryByPath "zip-archive.cabal" archive of 
       Nothing  -> assertFailure "zip-archive.cabal not found in archive"
       Just f   -> case contentsOfZipEntry f of
                        Left e   -> assertFailure e
                        Right c  -> assertEqual "for contents of zip-archive.cabal in archive" cabalContents c

testFromToZipArchive = TestCase $ do
  archive <- addFilesToZipArchive [OptRecursive] emptyZipArchive ["LICENSE", "Codec"]
  assertEqual "for (toZipArchive $ fromZipArchive archive)" archive (toZipArchive $ fromZipArchive archive)

testReadWriteZipEntry = TestCase $ do
  entry <- readZipEntry "zip-archive.cabal"
  writeZipEntry "test-temp/zip-archive.cabal" entry
  entry' <- readZipEntry "test-temp/zip-archive.cabal"
  let entry'' = entry' { eRelativePath = eRelativePath entry, eLastModified = eLastModified entry }
  assertEqual "for readZipEntry -> writeZipEntry -> readZipEntry" entry entry''

testAddFilesOptions = TestCase $ do
  archive1 <- addFilesToZipArchive [OptVerbose] emptyZipArchive ["LICENSE", "Codec"]
  archive2 <- addFilesToZipArchive [OptRecursive, OptVerbose] archive1 ["LICENSE", "Codec"]
  assertBool "for recursive and nonrecursive addFilesToZipArchive"
     (length (filesInZipArchive archive1) < length (filesInZipArchive archive2))

testDeleteEntries = TestCase $ do
  archive1 <- addFilesToZipArchive [] emptyZipArchive ["LICENSE", "Codec/"] 
  let archive2 = deleteEntryFromZipArchive "LICENSE" archive1
  let archive3 = deleteEntryFromZipArchive "Codec/" archive2
  assertEqual "for deleteFilesFromZipArchive" emptyZipArchive archive3

testExtractFiles = TestCase $ do
  createDirectory "test-temp/dir1"
  createDirectory "test-temp/dir1/dir2"
  let hiMsg = "hello there"
  let helloMsg = "Hello there. This file is very long.  Longer than 31 characters."
  writeFile "test-temp/dir1/hi" hiMsg
  writeFile "test-temp/dir1/dir2/hello" helloMsg
  archive <- addFilesToZipArchive [OptRecursive] emptyZipArchive ["test-temp/dir1"]
  removeDirectoryRecursive "test-temp/dir1"
  extractFilesFromZipArchive [OptVerbose] archive
  hi <- readFile "test-temp/dir1/hi"
  hello <- readFile "test-temp/dir1/dir2/hello"
  assertEqual "contents of test-temp/dir1/hi" hiMsg hi
  assertEqual "contents of test-temp/dir1/dir2/hello" helloMsg hello

