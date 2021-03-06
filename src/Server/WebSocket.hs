{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server.WebSocket
  ( WebSocketApi,
    ClientCommand,
    DataForClient,
    initChannel,
  )
where

import qualified Client.Room
import Control.Concurrent.Async as Async (async, waitEitherCancel)
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (catchJust)
import Data.Aeson (eitherDecode, encode)
import Data.Text (Text, pack)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Game
import Game (Color, Country)
import Game.Room (Room)
import qualified Game.Room as Room
import Network.WebSockets (DataMessage)
import qualified Network.WebSockets as WebSockets
import Network.WebSockets.Connection (Connection, receiveDataMessage, sendTextData)
import Servant.API.WebSocket
import Server.Serialization (tagToApiLabel)

type WebSocketApi = WebSocketPending

data Channel = Channel
  { -- reference to the room, shared accross all client channels.
    roomVar :: TVar Room,
    -- reference to the state. when a notification comes we need to read the
    -- current state to build the notification for the client.
    stateVar :: TVar State,
    -- the channel where updates to the room are published.
    roomUpdates :: TChan Room.State,
    -- connection to the client
    connection :: Connection
  }

data State
  = WaitingToJoin
  | InsideRoom Color

data ClientCommand
  = JoinRoom Color Text
  | StartGame
  | PaintCountry Color Country

deriveBoth defaultOptions {constructorTagModifier = tagToApiLabel} ''ClientCommand

data DataForClient
  = LobbyUpdate Client.Room.Lobby
  | RoomUpdate Client.Room.Room

deriveBoth defaultOptions {constructorTagModifier = tagToApiLabel} ''DataForClient

initChannel :: TVar Room -> Connection -> IO ()
initChannel roomVar connection = do
  let initialState = WaitingToJoin
  room <- STM.atomically (readTVar roomVar)
  roomUpdates <- STM.atomically (Room.subscribe room)
  stateVar <- STM.atomically (newTVar initialState)
  let channel =
        Channel
          { roomVar = roomVar,
            stateVar = stateVar,
            roomUpdates = roomUpdates,
            connection = connection
          }
  clientCommands <- async (clientCommandsLoop channel)
  notifications <- async $ do
    pushRoomUpdate connection (Room.state room) initialState
    notificationsLoop channel
  _ <-
    waitEitherCancel
      clientCommands
      notifications
  return ()

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

pushRoomUpdate :: Connection -> Room.State -> State -> IO ()
pushRoomUpdate connection roomState state =
  case roomUpdate roomState state of
    Nothing ->
      -- TODO: notify the client somehow?
      return ()
    Just dataForClient ->
      sendTextData connection (encode dataForClient)

roomUpdate :: Room.State -> State -> Maybe DataForClient
roomUpdate roomState channelState =
  case channelState of
    WaitingToJoin ->
      case Room.forClientInLobby roomState of
        Left _ ->
          Nothing
        Right lobby ->
          Just (LobbyUpdate lobby)
    InsideRoom color ->
      Just (RoomUpdate (Room.forClientInTheRoom color roomState))

data ReadResult
  = ConnectionClosed
  | InvalidMessage Text
  | Command ClientCommand

clientCommandsLoop :: Channel -> IO ()
clientCommandsLoop channel = do
  readResult <-
    catchJust
      socketExceptionHandler
      (fmap fromDataMessage (receiveDataMessage (connection channel)))
      (\errorEvent -> return errorEvent)
  case readResult of
    ConnectionClosed ->
      STM.atomically
        ( do
            finalState <- readTVar (stateVar channel)
            case finalState of
              WaitingToJoin ->
                return ()
              InsideRoom color -> do
                updateRoom
                  (return . (Room.disconnect color))
                  (roomVar channel)
        )
    InvalidMessage _ ->
      -- TODO: notify the client somehow?
      clientCommandsLoop channel
    Command cmd -> do
      STM.atomically
        ( updateRoom
            ( \roomState -> do
                state <- readTVar (stateVar channel)
                let (newState, newRoomState) = processCommand roomState state cmd
                writeTVar (stateVar channel) newState
                return newRoomState
            )
            (roomVar channel)
        )
      clientCommandsLoop channel

updateRoom :: (Room.State -> STM Room.State) -> TVar Room -> STM ()
updateRoom fn roomVar = do
  room <- readTVar roomVar
  newRoomState <- fn (Room.state room)
  let room' = room {Room.state = newRoomState}
  writeTVar roomVar room'
  Room.broadcastChanges room'

processCommand :: Room.State -> State -> ClientCommand -> (State, Room.State)
processCommand roomState state cmd =
  case cmd of
    JoinRoom color name ->
      case Room.join color name roomState of
        Left _ ->
          (state, roomState)
        Right roomState' ->
          (InsideRoom color, roomState')
    StartGame ->
      case Room.startGame roomState of
        Left _ ->
          (state, roomState)
        Right roomState' ->
          (state, roomState')
    PaintCountry color country ->
      case Room.updateGame (Game.paintCountry color country) roomState of
        Left _ ->
          (state, roomState)
        Right roomState' ->
          (state, roomState')

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
