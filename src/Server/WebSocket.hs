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
import Game.Room (Room)
import qualified Game.Room as Room
import qualified Network.WebSockets as WebSockets
import Network.WebSockets.Connection (Connection, PendingConnection, acceptRequest, receiveData, sendTextData, withPingThread)
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

runChannel :: Room -> Connection -> IO ()
runChannel room connection = do
  roomUpdates <- STM.atomically (Room.subscribe room)
  queue <- TChan.newTChanIO
  Async.mapConcurrently_
    id
    [ (notificationsLoop connection queue roomUpdates),
      (socketEventLoop connection queue),
      (updateLoop connection queue Channel.init)
    ]

notificationsLoop :: Connection -> Queue -> TChan Room.State -> IO ()
notificationsLoop connection queue roomUpdates = do
  roomState <- STM.atomically $ TChan.readTChan roomUpdates
  STM.atomically $ TChan.writeTChan queue (Channel.Update roomState)
  notificationsLoop connection queue roomUpdates

socketEventLoop :: Connection -> Queue -> IO ()
socketEventLoop connection queue = do
  event <-
    catchJust
      socketExceptionHandler
      (Channel.Received <$> receiveData connection)
      (\errorEvent -> return errorEvent)
  STM.atomically $ TChan.writeTChan queue event
  socketEventLoop connection queue

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
  nextEvent <- STM.atomically $ do
    isEmpty <- TChan.isEmptyTChan queue
    if isEmpty then STM.retry else TChan.readTChan queue
  let (updatedState, effect) = Channel.update state nextEvent
  case effect of
    Channel.NoEffect ->
      updateLoop connection queue updatedState
    Channel.SendToClient text -> do
      sendTextData connection text
      updateLoop connection queue updatedState
    Channel.HangUp ->
      return ()
