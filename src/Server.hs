{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( Server.run,
    APIRoutes,
  )
where

import Data.Maybe (fromMaybe)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Game
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment (lookupEnv)

type APIRoutes = "game" :> Get '[JSON] Game.State

deriveBoth defaultOptions {constructorTagModifier = Game.tagToApiLabel} ''Game.Country
deriveBoth defaultOptions ''Game.State

type Routes =
  APIRoutes
    :<|> "_build" :> Raw
    :<|> Raw

run :: IO ()
run = do
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
  return Game.init
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"
