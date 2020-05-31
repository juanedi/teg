{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( Routes,
    server,
  )
where

import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad.IO.Class (MonadIO)
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
        State.runSTM
          ( do
              chan <- State.playerUpdatesChannel state
              gameState <- State.readGameState state
              pure (gameState, chan)
          )
      yield (Game.playerState player currentGameState)
      notificationLoop playerChannel player

notificationLoop :: MonadIO m => TChan Game.State -> Player -> ConduitT () Game.LocalState m ()
notificationLoop playerChannel player = do
  gameState <- State.runSTM (readTChan playerChannel)
  yield $ Game.playerState player gameState
  notificationLoop playerChannel player
