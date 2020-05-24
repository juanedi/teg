module Main where

import Data.Maybe (fromMaybe)
import Lib
import System.Environment (lookupEnv)

main :: IO ()
main = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  putStrLn ("Started the application at port " ++ show port)
  startApp port
