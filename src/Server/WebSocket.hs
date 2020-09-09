{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( WebSocketApi,
    server,
  )
where

import qualified Channel
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TVar (readTVar)
import Control.Exception (catchJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text, pack)
import qualified Game.Room as Room
import Network.WebSockets (DataMessage)
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
          runChannel state connection

type Queue = TChan Channel.Event

enqueue :: Channel.Event -> Queue -> IO ()
enqueue event queue =
  STM.atomically (TChan.writeTChan queue event)

dequeue :: Queue -> STM Channel.Event
dequeue queue = do
  isEmpty <- TChan.isEmptyTChan queue
  if isEmpty then STM.retry else TChan.readTChan queue

runChannel :: Server.State.State -> Connection -> IO ()
runChannel state connection = do
  (room, roomUpdates, queue) <- STM.atomically $ do
    room <- readTVar (Server.State.roomVar state)
    roomUpdates <- Room.subscribe room
    queue <- TChan.newTChan
    return (room, roomUpdates, queue)
  Async.mapConcurrently_
    id
    [ -- channel state machine, consumes events produced by the other threads
      updateLoop state connection queue Channel.init,
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

data ReadResult
  = ConnectionClosed
  | InvalidMessage Text
  | Command Channel.ClientCommand

socketEventLoop :: Connection -> Queue -> IO ()
socketEventLoop connection queue = do
  readResult <-
    catchJust
      socketExceptionHandler
      (fmap fromDataMessage (receiveDataMessage connection))
      (\errorEvent -> return errorEvent)
  case readResult of
    ConnectionClosed ->
      return ()
    InvalidMessage _ ->
      -- TODO: notify the client somehow?
      socketEventLoop connection queue
    Command cmd -> do
      enqueue (Channel.Received cmd) queue
      socketEventLoop connection queue

fromDataMessage :: DataMessage -> ReadResult
fromDataMessage dataMessage =
  case dataMessage of
    WebSockets.Text bs _ ->
      case eitherDecode bs of
        Left err ->
          InvalidMessage (pack err)
        Right command ->
          Command command
    WebSockets.Binary _ ->
      InvalidMessage "The server doesn't support binary messages"

socketExceptionHandler :: WebSockets.ConnectionException -> Maybe ReadResult
socketExceptionHandler exception =
  case exception of
    WebSockets.CloseRequest _ _ ->
      Just ConnectionClosed
    WebSockets.ConnectionClosed ->
      Just ConnectionClosed
    _ ->
      -- TODO: not sure what other type of errors could come here. should we
      -- fail on more cases?
      Nothing

updateLoop :: Server.State.State -> Connection -> Queue -> Channel.State -> IO ()
updateLoop serverState connection queue state = do
  (updatedState, effect) <-
    STM.atomically
      ( do
          nextEvent <- dequeue queue
          Server.State.updateRoom
            ( \roomState ->
                let (updatedState, newRoomState, effect) = Channel.update roomState state nextEvent
                 in return ((updatedState, effect), newRoomState)
            )
            serverState
      )
  case effect of
    Channel.NoEffect ->
      updateLoop serverState connection queue updatedState
    Channel.SendToClient dataForClient -> do
      sendTextData connection (encode dataForClient)
      updateLoop serverState connection queue updatedState
