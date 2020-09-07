{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( WebSocketApi,
    server,
  )
where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Room
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan, dupTChan, writeTChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Exception (catch, catchJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Text (Text, pack)
import Game (Color, Country)
import Game.Room (Room)
import qualified Game.Room as Room
import qualified Network.WebSockets as WebSockets
import Network.WebSockets.Connection (Connection, PendingConnection, acceptRequest, receiveData, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket
import Server.State (State)
import qualified Server.State as State

type WebSocketApi = "ws" :> WebSocketPending

server :: State -> Server WebSocketApi
server state = clientChannel
  where
    clientChannel :: MonadIO m => PendingConnection -> m ()
    clientChannel pc =
      liftIO $ do
        connection <- acceptRequest pc
        room <- STM.atomically (readTVar (State.roomVar state))
        runChannel room connection

data ChannelState
  = WaitingToJoin
  | InsideRoom Color

-- data ClientCommand
--   = JoinRoom Color Text
--   | StartGame
--   | PaintCountry Color Country

type ClientCommand = Text

-- data DataForClient
--   = -- TODO: command responses would go here too
--     NewLobbyUpdate ConnectionStates
--   | NewRoomUpdate Client.Room.Room

type DataForClient = Text

data Event
  = Received ClientCommand
  | Update Room.State
  | ConnectionClosed

data Effect
  = NoEffect
  | SendToClient DataForClient
  | HangUp
  deriving (Show)

runChannel :: Room -> Connection -> IO ()
runChannel room connection =
  let initialState = WaitingToJoin
   in do
        roomUpdates <- STM.atomically (Room.subscribe room)
        queue <- TChan.newTChanIO
        Async.mapConcurrently_
          id
          [ (notificationsLoop connection queue roomUpdates),
            (socketEventLoop connection queue),
            (updateLoop connection queue initialState)
          ]

notificationsLoop :: Connection -> TChan Event -> TChan Room.State -> IO ()
notificationsLoop connection queue roomUpdates = do
  roomState <- STM.atomically $ TChan.readTChan roomUpdates
  STM.atomically $ TChan.writeTChan queue (Update roomState)
  notificationsLoop connection queue roomUpdates

socketEventLoop :: Connection -> TChan Event -> IO ()
socketEventLoop connection queue = do
  event <-
    catchJust
      socketExceptionHandler
      (Received <$> receiveData connection)
      (\errorEvent -> return errorEvent)
  STM.atomically $ TChan.writeTChan queue event
  socketEventLoop connection queue

socketExceptionHandler :: WebSockets.ConnectionException -> Maybe Event
socketExceptionHandler exception =
  case exception of
    WebSockets.CloseRequest _ _ ->
      Just ConnectionClosed
    WebSockets.ConnectionClosed ->
      Just ConnectionClosed
    _ ->
      Nothing

updateLoop :: Connection -> TChan Event -> ChannelState -> IO ()
updateLoop connection queue state = do
  nextEvent <- STM.atomically $ do
    isEmpty <- TChan.isEmptyTChan queue
    if isEmpty then STM.retry else TChan.readTChan queue
  let (updatedState, effect) = update state nextEvent
  case effect of
    NoEffect ->
      updateLoop connection queue updatedState
    SendToClient text -> do
      sendTextData connection text
      updateLoop connection queue updatedState
    HangUp ->
      return ()

update :: ChannelState -> Event -> (ChannelState, Effect)
update state event =
  case event of
    Received cmd ->
      (state, SendToClient cmd)
    Update _ ->
      (state, SendToClient "room update!")
    ConnectionClosed ->
      (state, HangUp)
