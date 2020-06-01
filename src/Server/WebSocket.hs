{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( Routes,
    server,
  )
where

import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (ConduitT, bracketP)
import Data.Conduit (yield)
import qualified Game
import Game (Player)
import Servant
import Servant.API.WebSocketConduit (WebSocketConduit)
import Server.State (State)
import qualified Server.State as State

type Routes =
  "socket" :> Capture "player" Player :> WebSocketConduit () Game.LocalState

server :: State -> Server Routes
server state =
  ( \player ->
      bracketP
        (playerConnected state player)
        (playerDisconnected state player)
        (startNotifications state player)
  )

playerConnected :: State -> Player -> IO (TChan Game.State)
playerConnected state player = do
  putStrLn "------------------ player connected"
  State.runSTM (State.playerUpdatesChannel state)

startNotifications :: MonadIO m => State -> Player -> TChan Game.State -> ConduitT () Game.LocalState m ()
startNotifications state player playerChannel = do
  firstState <- State.runSTM (State.readGameState state)
  notificationLoop player playerChannel firstState

notificationLoop :: MonadIO m => Player -> TChan Game.State -> Game.State -> ConduitT () Game.LocalState m ()
notificationLoop player playerChannel stateToNotify = do
  yield $ Game.playerState player stateToNotify
  newState <- State.runSTM (readTChan playerChannel)
  notificationLoop player playerChannel newState

playerDisconnected :: State -> Player -> TChan Game.State -> IO ()
playerDisconnected state player playerChannel =
  putStrLn "------------------ player disconnected"
