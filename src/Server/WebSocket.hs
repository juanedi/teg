{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( Routes,
    server,
  )
where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Room
import Conduit (ResourceT)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (readTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Data.Conduit (ConduitT, bracketP)
import Data.Conduit (yield)
import Game (Color)
import qualified Game.Room
import Game.Room (ClientChannel)
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
  "lobby" :> WebSocketConduit () ConnectionStates
    :<|> "game_updates" :> Capture "player" Color :> WebSocketConduit () Client.Room.Room

server :: State -> Server Routes
server state =
  lobby state
    :<|> gameUpdates state

lobby :: State -> ConduitT () ConnectionStates (ResourceT IO) ()
lobby state = do
  (room, chan) <-
    State.runSTM $
      do
        room <- (readTVar (State.roomVar state))
        chan <- Game.Room.subscribe room
        pure (room, chan)
  lobbyLoop chan (Game.Room.state room)

lobbyLoop :: STM.TChan Game.Room.State -> Game.Room.State -> ConduitT () ConnectionStates (ResourceT IO) ()
lobbyLoop chan roomState = do
  let connectionStates = Game.Room.connectionStates roomState
  yield connectionStates
  roomState' <- State.runSTM (readTChan chan)
  lobbyLoop chan roomState'

gameUpdates :: State -> Color -> UpdatesConduit
gameUpdates state player =
  bracketP
    (playerConnected state player)
    (playerDisconnected state player)
    (startNotifications state player)

playerConnected :: State -> Color -> IO ClientChannel
playerConnected state player = do
  maybeChannel <-
    STM.atomically $
      State.updateRoom
        (Game.Room.playerConnected player)
        state
  case maybeChannel of
    Nothing -> fail "Could not connect!"
    Just channel -> do
      putStrLn ("------------------ player connected: " ++ show player)
      pure channel

startNotifications :: State -> Color -> ClientChannel -> UpdatesConduit
startNotifications state player playerChannel = do
  -- we want to send an initial update right away without waiting for there to
  -- be a change in the room
  room <- State.runSTM (readTVar (State.roomVar state))
  notificationLoop player playerChannel (Game.Room.state room)

notificationLoop :: Color -> ClientChannel -> Game.Room.State -> UpdatesConduit
notificationLoop player playerChannel roomState = do
  yield (Game.Room.clientState player roomState)
  roomState' <- State.runSTM (readTChan playerChannel)
  notificationLoop player playerChannel roomState'

playerDisconnected :: State -> Color -> ClientChannel -> IO ()
playerDisconnected state player playerChannel =
  -- TODO: cleanup socket and pause game!
  putStrLn ("------------------ player disconnected: " ++ show player)
