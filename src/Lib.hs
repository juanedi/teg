{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( runServer,
    runCodegen,
    app,
  )
where

import Data.Maybe (fromMaybe)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Game
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Elm (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, generateElmModuleWith)
import System.Environment (lookupEnv)
import qualified System.Process
import System.Process (cwd, proc)

type APIRoutes = "game" :> Get '[JSON] Game.State

type Routes =
  APIRoutes
    :<|> "_build" :> Raw
    :<|> Raw

runServer :: IO ()
runServer = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port
            $ setLogger logger
            $ defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy Routes
api = Proxy

server :: Server Routes
server =
  return Game.new
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

deriveBoth defaultOptions {constructorTagModifier = Game.tagToApiLabel} ''Game.Country
deriveBoth defaultOptions ''Game.State

runCodegen :: IO ()
runCodegen =
  let outputDir = "./ui/generated"
   in do
        putStrLn "Deleting old generated code..."
        putStrLn "Generating Elm code from API..."
        generateElmModuleWith
          defElmOptions
          ["Api"]
          defElmImports
          outputDir
          [ DefineElm (Proxy :: Proxy Game.State),
            DefineElm (Proxy :: Proxy Game.Country)
          ]
          (Proxy :: Proxy APIRoutes)
        putStrLn "Formatting generated code using elm-format..."
        System.Process.createProcess (proc "elm-format" ["Api.elm", "--yes"]) {cwd = Just outputDir}
        putStrLn "Done!"
