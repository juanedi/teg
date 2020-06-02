{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( Routes,
    server,
  )
where

import qualified Client.Room
import Conduit (ResourceT)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Data.Conduit (ConduitT, bracketP)
import Data.Conduit (yield)
import Game (Player)
import Game.Room (Room)
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

playerConnected :: State -> Player -> IO (TChan Room)
playerConnected state player = do
  putStrLn "------------------ player connected"
  State.runSTM (State.playerUpdatesChannel state)

startNotifications :: State -> Player -> TChan Room -> UpdatesConduit
startNotifications state player playerChannel = do
  firstState <- State.runSTM (State.readRoom state)
  notificationLoop player playerChannel firstState

notificationLoop :: Player -> TChan Room -> Room -> UpdatesConduit
notificationLoop player playerChannel stateToNotify = do
  yield $ Game.Room.clientState player stateToNotify
  newState <- State.runSTM (readTChan playerChannel)
  notificationLoop player playerChannel newState

playerDisconnected :: State -> Player -> TChan Room -> IO ()
playerDisconnected state player playerChannel =
  putStrLn "------------------ player disconnected"
