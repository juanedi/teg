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
  { -- reference to the room, shared accross all client channels.
    roomVar :: TVar Room.Room,
    -- reference to the state. when a notification comes we need to read the
    -- current state to build the notification for the client.
    stateVar :: TVar Channel.State,
    -- the channel where updates to the room are published.
    roomUpdates :: TChan Room.State,
    -- connection to the client
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
  stateVar <- STM.atomically (newTVar initialState)
  let channel =
        Channel
          { roomVar = roomVar,
            stateVar = stateVar,
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
  (roomState, state) <-
    STM.atomically
      ( do
          roomState <- TChan.readTChan (roomUpdates channel)
          state <- readTVar (stateVar channel)
          return (roomState, state)
      )
  pushRoomUpdate (connection channel) roomState state
  notificationsLoop channel

pushRoomUpdate :: Connection -> Room.State -> Channel.State -> IO ()
pushRoomUpdate connection roomState state =
  case Channel.roomNotification roomState state of
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
processCommand roomVar stateVar cmd =
  STM.atomically $
    do
      room <- readTVar roomVar
      state <- readTVar stateVar
      let (newState, newRoomState) =
            Channel.update
              (Room.state room)
              state
              cmd
      let room' = room {Room.state = newRoomState}
      writeTVar roomVar room'
      writeTVar stateVar newState
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
