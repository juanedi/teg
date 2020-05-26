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
import qualified Game
import Game (Country, Player)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Server.State (State)
import qualified Server.State as State
import System.Environment (lookupEnv)

type APIRoutes =
  "state" :> Get '[JSON] Game.State
    :<|> "paint" :> ReqBody '[JSON] (Player, Country) :> Post '[JSON] Game.State

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
  ( getState serverState
      :<|> paintCountry serverState
  )
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

getState :: State -> Handler Game.State
getState serverState =
  liftIO (State.read serverState)

paintCountry :: State -> (Player, Country) -> Handler Game.State
paintCountry serverState (player, country) =
  liftIO (State.update (Game.paintCountry player country) serverState)
