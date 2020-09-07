{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( WebSocketApi,
    server,
  )
where

import qualified Channel
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TVar (readTVar)
import Control.Exception (catchJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (eitherDecode)
import Data.Text (pack)
import Game.Room (Room)
import qualified Game.Room as Room
import Network.WebSockets (DataMessage (..))
import qualified Network.WebSockets as WebSockets
import Network.WebSockets.Connection (Connection, PendingConnection, acceptRequest, receiveDataMessage, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket
import qualified Server.State

type WebSocketApi = "ws" :> WebSocketPending

server :: Server.State.State -> Server WebSocketApi
server state = clientChannel
  where
    clientChannel :: MonadIO m => PendingConnection -> m ()
    clientChannel pc =
      liftIO $ do
        connection <- acceptRequest pc
        withPingThread connection 10 (return ()) $ do
          room <- STM.atomically (readTVar (Server.State.roomVar state))
          runChannel room connection

type Queue = TChan Channel.Event

enqueue :: Channel.Event -> Queue -> IO ()
enqueue event queue =
  STM.atomically (TChan.writeTChan queue event)

dequeue :: Queue -> IO Channel.Event
dequeue queue =
  STM.atomically $ do
    isEmpty <- TChan.isEmptyTChan queue
    if isEmpty then STM.retry else TChan.readTChan queue

runChannel :: Room -> Connection -> IO ()
runChannel room connection = do
  roomUpdates <- STM.atomically (Room.subscribe room)
  queue <- TChan.newTChanIO
  Async.mapConcurrently_
    id
    [ -- channel state machine, consumes events produced by the other threads
      updateLoop connection queue Channel.init,
      -- produces events based on messages received via the websocket
      socketEventLoop connection queue,
      -- produces events based room updates
      do
        enqueue (Channel.Update (Room.state room)) queue
        notificationsLoop connection queue roomUpdates
    ]

notificationsLoop :: Connection -> Queue -> TChan Room.State -> IO ()
notificationsLoop connection queue roomUpdates = do
  roomState <- STM.atomically $ TChan.readTChan roomUpdates
  enqueue (Channel.Update roomState) queue
  notificationsLoop connection queue roomUpdates

socketEventLoop :: Connection -> Queue -> IO ()
socketEventLoop connection queue = do
  event <-
    catchJust
      socketExceptionHandler
      (fmap fromDataMessage (receiveDataMessage connection))
      (\errorEvent -> return errorEvent)
  enqueue event queue
  socketEventLoop connection queue

fromDataMessage :: DataMessage -> Channel.Event
fromDataMessage dataMessage =
  case dataMessage of
    Text bs _ ->
      case eitherDecode bs of
        Left err ->
          Channel.ReceivedInvalidMessage (pack err)
        Right command ->
          Channel.Received command
    Binary _ ->
      Channel.ReceivedInvalidMessage "The server doesn't support binary messages"

socketExceptionHandler :: WebSockets.ConnectionException -> Maybe Channel.Event
socketExceptionHandler exception =
  case exception of
    WebSockets.CloseRequest _ _ ->
      Just Channel.ConnectionClosed
    WebSockets.ConnectionClosed ->
      Just Channel.ConnectionClosed
    _ ->
      Nothing

updateLoop :: Connection -> Queue -> Channel.State -> IO ()
updateLoop connection queue state = do
  nextEvent <- dequeue queue
  let (updatedState, effect) = Channel.update state nextEvent
  case effect of
    Channel.NoEffect ->
      updateLoop connection queue updatedState
    Channel.SendToClient text -> do
      sendTextData connection text
      updateLoop connection queue updatedState
    Channel.HangUp ->
      return ()
