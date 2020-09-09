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
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
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

type Queue = TChan Channel.ClientCommand

enqueue :: Channel.ClientCommand -> Queue -> IO ()
enqueue cmd queue =
  STM.atomically (TChan.writeTChan queue cmd)

dequeue :: Queue -> STM Channel.ClientCommand
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
  let initialState = Channel.init
  channelStateVar <- STM.atomically (newTVar initialState)
  Async.mapConcurrently_
    id
    [ -- consumes client commannds from the queue and processes them
      updateLoop state connection queue channelStateVar,
      -- reads messages from the websocket and enqueues them
      clientCommandsLoop connection queue,
      -- pushes changes to the room state to the client
      do
        pushRoomUpdate connection (Room.state room) initialState
        notificationsLoop connection roomUpdates channelStateVar
    ]

notificationsLoop :: Connection -> TChan Room.State -> TVar Channel.State -> IO ()
notificationsLoop connection roomUpdates channelStateVar = do
  (roomState, channelState) <-
    STM.atomically
      ( do
          roomState <- TChan.readTChan roomUpdates
          channelState <- readTVar channelStateVar
          return (roomState, channelState)
      )
  pushRoomUpdate connection roomState channelState
  notificationsLoop connection roomUpdates channelStateVar

pushRoomUpdate :: Connection -> Room.State -> Channel.State -> IO ()
pushRoomUpdate connection roomState channelState =
  case Channel.roomNotification roomState channelState of
    Nothing ->
      -- TODO: notify the client somehow?
      return ()
    Just dataForClient ->
      sendTextData connection (encode dataForClient)

data ReadResult
  = ConnectionClosed
  | InvalidMessage Text
  | Command Channel.ClientCommand

clientCommandsLoop :: Connection -> Queue -> IO ()
clientCommandsLoop connection queue = do
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
      clientCommandsLoop connection queue
    Command cmd -> do
      enqueue cmd queue
      clientCommandsLoop connection queue

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

updateLoop :: Server.State.State -> Connection -> Queue -> TVar Channel.State -> IO ()
updateLoop serverState connection queue channelStateVar = do
  STM.atomically $
    do
      channelState <- readTVar channelStateVar
      nextCommand <- dequeue queue
      room <- readTVar (Server.State.roomVar serverState)
      let (newChannelState, newRoomState) =
            Channel.update
              (Room.state room)
              channelState
              nextCommand
      let room' = room {Room.state = newRoomState}
      writeTVar (Server.State.roomVar serverState) room'
      writeTVar channelStateVar newChannelState
      Room.broadcastChanges room'
      return ()
  updateLoop serverState connection queue channelStateVar
