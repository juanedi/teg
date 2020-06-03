{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( Routes,
    server,
  )
where

import qualified Client.Room
import Conduit (ResourceT)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (readTChan)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Data.Conduit (ConduitT, bracketP)
import Data.Conduit (yield)
import Game (Player)
import Game.Room (ClientChannel)
import qualified Game.Room
import Servant
import Servant.API.WebSocketConduit (WebSocketConduit)
import Server.State (State)
import qualified Server.State as State

type UpdatesConduit =
  ConduitT
    -- type of input values. nothing going on here since we don't get updates
    -- from clients on the websocket.
    ()
    -- the type of values we stream to the clients.
    Client.Room.Room
    -- the monad we are running on. allows for IO via MonadIO and
    -- acquiring/releasing resources via ResourceT (which we use to track
    -- connection states).
    (ResourceT IO)
    -- the resulting value of the computation. nothing relevant here since we
    -- don't operate on the result.
    ()

type Routes =
  "socket" :> Capture "player" Player :> WebSocketConduit () Client.Room.Room

server :: State -> Server Routes
server state =
  ( \player ->
      bracketP
        (playerConnected state player)
        (playerDisconnected state player)
        (startNotifications state player)
  )

playerConnected :: State -> Player -> IO ClientChannel
playerConnected state player = do
  maybeChannel <-
    STM.atomically
      ( do
          room <- readTVar (State.roomVar state)
          (room', maybeChannel) <- Game.Room.playerConnected player room
          writeTVar (State.roomVar state) room'
          -- broadcast to let other clients know that a new player joined.
          -- TODO: there's some duplication between this and State.updateRoom.
          Game.Room.broadcastChanges room'
          pure maybeChannel
      )
  case maybeChannel of
    Nothing -> fail "Could not connect!"
    Just channel -> do
      pure channel

startNotifications :: State -> Player -> ClientChannel -> UpdatesConduit
startNotifications state player playerChannel = do
  -- we want to send an initial update right away without waiting for there to
  -- be a change in the room
  room <- State.runSTM (readTVar (State.roomVar state))
  notificationLoop player playerChannel (Game.Room.state room)

notificationLoop :: Player -> ClientChannel -> Game.Room.State -> UpdatesConduit
notificationLoop player playerChannel roomState = do
  yield (Game.Room.clientState player roomState)
  roomState' <- State.runSTM (readTChan playerChannel)
  notificationLoop player playerChannel roomState'

playerDisconnected :: State -> Player -> ClientChannel -> IO ()
playerDisconnected state player playerChannel =
  -- TODO: cleanup!
  putStrLn "------------------ player disconnected"
