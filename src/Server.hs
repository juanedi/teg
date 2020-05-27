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
  "join" :> Post '[JSON] Game.LocalState
    :<|> "state" :> ReqBody '[JSON] Player :> Get '[JSON] Game.LocalState
    :<|> "paint" :> ReqBody '[JSON] (Player, Country) :> Post '[JSON] Game.LocalState

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
  ( join serverState
      :<|> getState serverState
      :<|> paintCountry serverState
  )
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

join :: State -> Handler Game.LocalState
join serverState =
  liftIO
    ( State.update_
        ( \gameState ->
            let (player, updatedGameState) = Game.join gameState
             in (Game.playerState player updatedGameState, gameState)
        )
        serverState
    )

getState :: State -> Player -> Handler Game.LocalState
getState serverState player =
  liftIO
    ( State.update_
        ( \gameState ->
            (Game.playerState player gameState, gameState)
        )
        serverState
    )

paintCountry :: State -> (Player, Country) -> Handler Game.LocalState
paintCountry serverState (player, country) =
  liftIO
    ( State.update_
        ( \gameState ->
            let updatedGameState = Game.paintCountry player country gameState
             in ( Game.playerState player updatedGameState,
                  updatedGameState
                )
        )
        serverState
    )
