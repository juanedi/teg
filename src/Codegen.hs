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
import Elm.Module
import qualified Game
import qualified Server.Flags
import qualified Server.WebSocket as WebSocket
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
                [ DefineElm (Proxy :: Proxy Client.ConnectionStates.ConnectionStates),
                  DefineElm (Proxy :: Proxy Client.Game.Game),
                  DefineElm (Proxy :: Proxy Client.Game.Instructions),
                  DefineElm (Proxy :: Proxy Client.Room.Lobby),
                  DefineElm (Proxy :: Proxy Client.Room.Room),
                  DefineElm (Proxy :: Proxy Game.Color),
                  DefineElm (Proxy :: Proxy Game.Country),
                  DefineElm (Proxy :: Proxy Server.Flags.Flags),
                  DefineElm (Proxy :: Proxy WebSocket.ClientCommand),
                  DefineElm (Proxy :: Proxy WebSocket.DataForClient)
                ]

        Data.Text.IO.writeFile (joinPath [outputDir, fileName]) (pack content)
        putStrLn "Formatting generated code using elm-format..."
        _ <- createProcess (proc "elm-format" [fileName, "--yes"]) {cwd = Just outputDir}
        putStrLn "Done!"
