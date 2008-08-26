------------------------------------------------------------------------
-- Zip.hs
-- Copyright (c) 2008 John MacFarlane
-- License     : GPL 2 (see LICENSE)
--
-- This is a demonstration of the use of the 'Codec.Archive.Zip' library.
-- It duplicates some of the functionality of the 'zip' command-line
-- program.
------------------------------------------------------------------------

import Codec.Archive.Zip
import System.IO
import qualified Data.ByteString.Lazy as B
import System.Exit
import System.Environment
import System.Directory
import System.Console.GetOpt
import Control.Monad ( when )

data Flag 
  = Quiet 
  | Version
  | Decompress
  | Recursive
  | Remove
  | List
  | Help
  deriving (Eq, Show, Read)

options :: [OptDescr Flag]
options =
   [ Option ['d']   ["decompress"] (NoArg Decompress)    "decompress (unzip)"
   , Option ['r']   ["recursive"]  (NoArg Recursive)     "recursive"
   , Option ['R']   ["remove"]     (NoArg Remove)        "remove"
   , Option ['l']   ["list"]       (NoArg List)          "list"
   , Option ['v']   ["version"]    (NoArg Version)       "version"
   , Option ['q']   ["quiet"]      (NoArg Quiet)         "quiet"
   , Option ['h']   ["help"]       (NoArg Help)          "help"
   ]

main :: IO ()
main = do
  argv <- getArgs
  progname <- getProgName
  let header = "Usage: " ++ progname ++ " [OPTION...] archive files..."
  (opts, args) <- case getOpt Permute options argv of
      (o, _, _)      | Version `elem` o -> putStrLn "version 0.1" >> exitWith ExitSuccess
      (o, _, _)      | Help `elem` o    -> error $ usageInfo header options
      (o, (a:as), [])                   -> return (o, a:as)
      (_, _, errs)                      -> error $ concat errs ++ "\n" ++ usageInfo header options
  let verbosity = if Quiet `elem` opts then [] else [OptVerbose]
  let cmd = take 1 $ filter (`notElem` [Quiet, Help, Version]) opts
  let cmd' = if null cmd
                then Recursive
                else head cmd
  let (archivePath : files) = args
  exists <- doesFileExist archivePath
  archive <- if exists
                then readZipArchive archivePath
                else return emptyZipArchive
  case cmd' of
       Decompress  -> extractFilesFromZipArchive verbosity archive  
       Remove      -> do tempDir <- getTemporaryDirectory
                         (tempArchivePath, tempArchive) <- openTempFile tempDir "zip" 
                         B.hPut tempArchive $ fromZipArchive $ 
                                              foldr deleteEntryFromZipArchive archive files
                         hClose tempArchive
                         copyFile tempArchivePath archivePath
                         removeFile tempArchivePath
       List        -> mapM_ putStrLn $ filesInZipArchive archive
       Recursive   -> do when (null files) $ error "No files specified."
                         tempDir <- getTemporaryDirectory
                         (tempArchivePath, tempArchive) <- openTempFile tempDir "zip" 
                         addFilesToZipArchive (verbosity ++ [OptRecursive]) archive files >>= 
                            B.hPut tempArchive . fromZipArchive
                         hClose tempArchive
                         copyFile tempArchivePath archivePath
                         removeFile tempArchivePath
       _           -> error "Unknown command"
