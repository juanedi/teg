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

type WebSocketApi = "ws" :> WebSocketPending

data Channel = Channel
  { roomVar :: TVar Room.Room,
    stateVar :: TVar Channel.State,
    roomUpdates :: TChan Room.State,
    connection :: Connection
  }

server :: TVar Room.Room -> Server WebSocketApi
server roomVar = clientChannel
  where
    clientChannel :: MonadIO m => PendingConnection -> m ()
    clientChannel pc =
      liftIO $ do
        connection <- acceptRequest pc
        withPingThread connection 10 (return ()) $ do
          initChannel roomVar connection

initChannel :: TVar Room.Room -> Connection -> IO ()
initChannel roomVar connection = do
  room <- STM.atomically (readTVar roomVar)
  let initialState = Channel.init
  roomUpdates <- STM.atomically (Room.subscribe room)
  channelStateVar <- STM.atomically (newTVar initialState)
  let channel =
        Channel
          { roomVar = roomVar,
            stateVar = channelStateVar,
            roomUpdates = roomUpdates,
            connection = connection
          }
  Async.mapConcurrently_
    id
    [ clientCommandsLoop channel,
      -- pushes changes to the room state to the client
      do
        pushRoomUpdate connection (Room.state room) initialState
        notificationsLoop channel
    ]

notificationsLoop :: Channel -> IO ()
notificationsLoop channel = do
  (roomState, channelState) <-
    STM.atomically
      ( do
          roomState <- TChan.readTChan (roomUpdates channel)
          channelState <- readTVar (stateVar channel)
          return (roomState, channelState)
      )
  pushRoomUpdate (connection channel) roomState channelState
  notificationsLoop channel

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

clientCommandsLoop :: Channel -> IO ()
clientCommandsLoop channel = do
  readResult <-
    catchJust
      socketExceptionHandler
      (fmap fromDataMessage (receiveDataMessage (connection channel)))
      (\errorEvent -> return errorEvent)
  case readResult of
    ConnectionClosed ->
      return ()
    InvalidMessage _ ->
      -- TODO: notify the client somehow?
      clientCommandsLoop channel
    Command cmd -> do
      processCommand (roomVar channel) (stateVar channel) cmd
      clientCommandsLoop channel

processCommand :: TVar Room.Room -> TVar Channel.State -> Channel.ClientCommand -> IO ()
processCommand roomVar channelStateVar cmd =
  STM.atomically $
    do
      room <- readTVar roomVar
      channelState <- readTVar channelStateVar
      let (newChannelState, newRoomState) =
            Channel.update
              (Room.state room)
              channelState
              cmd
      let room' = room {Room.state = newRoomState}
      writeTVar roomVar room'
      writeTVar channelStateVar newChannelState
      Room.broadcastChanges room'
      return ()

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
