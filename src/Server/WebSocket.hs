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

type WebSocketApi = "ws" :> WebSocketPending

server :: Server WebSocketApi
server = clientChannel
  where
    clientChannel :: MonadIO m => PendingConnection -> m ()
    clientChannel pc =
      do
        c <- liftIO $ acceptRequest pc
        liftIO
          $ withPingThread c 10 (return ())
          $ Async.concurrently
            ( liftIO . forM_ [1 ..] $
                \i ->
                  sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
            )
            ( liftIO . forM_ [1 ..] $
                \i -> do
                  m <- receiveData c :: IO Text
                  sendTextData c m
            )
        return ()

data ChannelState
  = WaitingToJoin
  | InsideRoom Color

-- data ClientCommand
--   = JoinRoom Color Text
--   | StartGame
--   | PaintCountry Color Country

type ClientCommand = Text

data DataForClient
  = -- TODO: command responses would go here too
    NewLobbyUpdate ConnectionStates
  | NewRoomUpdate Client.Room.Room

data Event
  = Received ClientCommand
  | Update Room.State
  | ConnectionClosed

runChannel :: Room -> Connection -> IO ()
runChannel room connection = do
  state <- STM.atomically $ newTVar WaitingToJoin
  roomUpdates <- STM.atomically (Room.subscribe room)
  queue <- TChan.newTChanIO
  Async.concurrently
    (notificationsLoop queue roomUpdates connection)
    (socketEventLoop queue connection)
  -- TODO: loop over the queue processing events!
  return ()

notificationsLoop :: TChan Event -> TChan Room.State -> Connection -> IO ()
notificationsLoop queue roomUpdates connection = do
  roomState <- STM.atomically $ TChan.readTChan roomUpdates
  STM.atomically $ TChan.writeTChan queue (Update roomState)
  notificationsLoop queue roomUpdates connection

socketEventLoop :: TChan Event -> Connection -> IO ()
socketEventLoop queue connection = do
  event <-
    catchJust
      socketExceptionHandler
      (Received <$> receiveData connection)
      (\errorEvent -> return errorEvent)
  STM.atomically $ TChan.writeTChan queue event
  socketEventLoop queue connection

socketExceptionHandler :: WebSockets.ConnectionException -> Maybe Event
socketExceptionHandler exception =
  case exception of
    WebSockets.CloseRequest _ _ ->
      Just ConnectionClosed
    WebSockets.ConnectionClosed ->
      Just ConnectionClosed
    _ ->
      Nothing
