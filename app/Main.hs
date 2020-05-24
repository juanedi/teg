module Main where

import qualified Lib
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--server"]  -> Lib.runServer
    ["--codegen"] -> Lib.runCodegen
    _             -> System.Exit.die "Invalid arguments!"
