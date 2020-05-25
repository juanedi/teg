{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( Server.run,
    APIRoutes,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Game
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Server.State (State)
import qualified Server.State as State
import System.Environment (lookupEnv)

type APIRoutes = "state" :> Get '[JSON] Game.State

-- :<|> "paint" :> ReqBody '[JSON] Country :> Post '[POST] Bool

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
  serverState <- State.init
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port
            $ setLogger logger
            $ defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings (app serverState)

app :: State -> Application
app serverState = serve api (server serverState)

api :: Proxy Routes
api = Proxy

server :: State -> Server Routes
server serverState =
  getState serverState
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

getState :: State -> Handler Game.State
getState serverState = do
  gameState <- liftIO (State.read serverState)
  return gameState
