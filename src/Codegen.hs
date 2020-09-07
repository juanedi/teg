{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Codegen (run) where

import qualified Client.ConnectionStates
import qualified Client.Game
import qualified Client.Room
import Data.Proxy
import Data.Text (pack)
import qualified Data.Text.IO
import Elm.Derive
import Elm.Module
import qualified Game
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath)
import System.Process (createProcess, cwd, proc)

run :: IO ()
run =
  let outputDir = "./ui/generated"
      fileName = "Api.elm"
   in do
        putStrLn "Deleting old generated code..."
        putStrLn "Generating Elm code from API..."
        createDirectoryIfMissing True outputDir
        let content =
              makeElmModule
                "Api"
                [ DefineElm (Proxy :: Proxy Game.Country),
                  DefineElm (Proxy :: Proxy Game.Color),
                  DefineElm (Proxy :: Proxy Client.ConnectionStates.ConnectionStates),
                  DefineElm (Proxy :: Proxy Client.Game.Game),
                  DefineElm (Proxy :: Proxy Client.Game.Instructions),
                  DefineElm (Proxy :: Proxy Client.Room.Room)
                ]

        Data.Text.IO.writeFile (joinPath [outputDir, fileName]) (pack content)
        putStrLn "Formatting generated code using elm-format..."
        createProcess (proc "elm-format" [fileName, "--yes"]) {cwd = Just outputDir}
        putStrLn "Done!"
