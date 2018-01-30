module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  {
    hookedPrograms = [ simpleProgram "zip", simpleProgram "unzip"]
  }
