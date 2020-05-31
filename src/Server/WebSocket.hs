{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (ConduitT)
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
server state = socketHandler
  where
    socketHandler :: MonadIO m => Player -> ConduitT () Game.LocalState m ()
    socketHandler player = do
      (currentGameState, playerChannel) <-
        runSTM
          ( do
              chan <- dupTChan (State.broadcastChannel state)
              gameState <- readTVar (State.gameState state)
              pure (gameState, chan)
          )
      yield (Game.playerState player currentGameState)
      notificationLoop playerChannel player

notificationLoop :: MonadIO m => TChan Game.State -> Player -> ConduitT () Game.LocalState m ()
notificationLoop playerChannel player = do
  gameState <- runSTM (readTChan playerChannel)
  yield $ Game.playerState player gameState
  notificationLoop playerChannel player

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . STM.atomically
