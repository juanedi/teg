module Main where

import qualified Codegen
import qualified Server
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--server"] -> Server.run
    ["--codegen"] -> Codegen.run
    _ -> System.Exit.die "Invalid arguments!"
