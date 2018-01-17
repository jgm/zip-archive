------------------------------------------------------------------------
-- Zip.hs
-- Copyright (c) 2008 John MacFarlane
-- License     : BSD3 (see LICENSE)
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
import Control.Applicative ( (<$>) )
import Data.Version ( showVersion )
import Paths_zip_archive ( version )
import Debug.Trace ( traceShowId )

data Flag
  = Quiet
  | Version
  | Decompress
  | Recursive
  | Remove
  | List
  | Debug
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
   , Option []      ["debug"]      (NoArg Debug)         "debug output"
   , Option ['h']   ["help"]       (NoArg Help)          "help"
   ]

quit :: Bool -> String -> IO a
quit failure msg = do
  hPutStr stderr msg
  _ <- exitWith $ if failure
                     then ExitFailure 1
                     else ExitSuccess
  return undefined

main :: IO ()
main = do
  argv <- getArgs
  progname <- getProgName
  let header = "Usage: " ++ progname ++ " [OPTION...] archive files..."
  (opts, args) <- case getOpt Permute options argv of
      (o, _, _)      | Version `elem` o -> do
        putStrLn ("version " ++ showVersion version)
        exitWith ExitSuccess
      (o, _, _)      | Help `elem` o    -> quit False $ usageInfo header options
      (o, (a:as), [])                   -> return (o, a:as)
      (_, [], [])                       -> quit True $ usageInfo header options
      (_, _, errs)                      -> quit True $ concat errs ++ "\n" ++ usageInfo header options
  let verbosity = if Quiet `elem` opts then [] else [OptVerbose]
  let debug = Debug `elem` opts
  let cmd = case filter (`notElem` [Quiet, Help, Version, Debug]) opts of
                  []    -> Recursive
                  (x:_) -> x
  let (archivePath : files) = args
  exists <- doesFileExist archivePath
  archive <- if exists
                then toArchive <$> B.readFile archivePath
                else return emptyArchive
  let showArchiveIfDebug x = if debug
                                then traceShowId x
                                else x
  case cmd of
       Decompress  -> extractFilesFromArchive verbosity $ showArchiveIfDebug archive
       Remove      -> do tempDir <- getTemporaryDirectory
                         (tempArchivePath, tempArchive) <- openTempFile tempDir "zip"
                         B.hPut tempArchive $ fromArchive $ showArchiveIfDebug $
                                              foldr deleteEntryFromArchive archive files
                         hClose tempArchive
                         copyFile tempArchivePath archivePath
                         removeFile tempArchivePath
       List        -> mapM_ putStrLn $ filesInArchive $ showArchiveIfDebug archive
       Recursive   -> do when (null files) $ error "No files specified."
                         tempDir <- getTemporaryDirectory
                         (tempArchivePath, tempArchive) <- openTempFile tempDir "zip"
                         addFilesToArchive (verbosity ++ [OptRecursive]) archive files >>=
                            B.hPut tempArchive . fromArchive . showArchiveIfDebug
                         hClose tempArchive
                         copyFile tempArchivePath archivePath
                         removeFile tempArchivePath
       _           -> error $ "Unknown command " ++ show cmd
