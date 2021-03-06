{-# LANGUAGE InstanceSigs #-}

module Game.Room
  ( Room,
    Id (..),
    State,
    Game.Room.init,
    subscribe,
    join,
    disconnect,
    startGame,
    broadcastChanges,
    updateGame,
    forClientInLobby,
    forClientInTheRoom,
    state,
  )
where

import qualified Client.ConnectionStates as ConnectionStates
import qualified Client.Room
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, writeTChan)
import Data.Text (Text, unpack)
import qualified Game
import Game.Color (Color)
import qualified Game.Color as Color
import Game.TurnList (TurnList)
import qualified Game.TurnList as TurnList
import Result (Error (..))
import Web.HttpApiData (FromHttpApiData (..))

newtype Id = Id Text deriving (Eq, Ord)

instance FromHttpApiData Id where
  parseUrlPiece :: Text -> Either Text Id
  parseUrlPiece fragment =
    Right (Id fragment)

instance Show Id where
  show :: Id -> String
  show (Id id) = unpack id

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers [(Color, Text)]
  | Started Game.State
  | Paused [Color] Game.State
  deriving (Eq)

init :: STM Room
init =
  do
    broadcastChannel <- newBroadcastTChan
    pure
      Room
        { broadcastChannel = broadcastChannel,
          state = WaitingForPlayers []
        }

subscribe :: Room -> STM (TChan State)
subscribe room =
  dupTChan (broadcastChannel room)

freeSlots :: [(Color, Text)] -> [Color]
freeSlots connectedPlayers =
  filter
    (\color -> not (isSlotTaken color connectedPlayers))
    Color.all

isSlotTaken :: Color -> [(Color, Text)] -> Bool
isSlotTaken color connectedPlayers =
  case lookup color connectedPlayers of
    Just _ ->
      True
    Nothing ->
      False

join :: Color -> Text -> State -> Either Error State
join color name state =
  case state of
    WaitingForPlayers connectedPlayers ->
      if isSlotTaken color connectedPlayers
        then Left (InvalidMove "That slot has already been taken")
        else Right (WaitingForPlayers ((color, name) : connectedPlayers))
    Started _ ->
      Left (InvalidMove "Trying to join a game that has already started")
    Paused missingPlayers gameState ->
      if elem color missingPlayers
        then case filter (\c -> c /= color) missingPlayers of
          [] ->
            Right (Started gameState)
          missingPlayers' ->
            Right (Paused missingPlayers' gameState)
        else Left (InvalidMove "The slot is not currently available in this game")

disconnect :: Color -> State -> State
disconnect color state =
  case state of
    WaitingForPlayers connectedPlayers ->
      WaitingForPlayers (filter (\(c, _) -> c /= color) connectedPlayers)
    Started gameState ->
      -- TODO: get name of the player for this color
      Paused [color] gameState
    Paused missingPlayers gameState ->
      -- TODO: get name of the player for this color
      Paused (color : missingPlayers) gameState

forClientInLobby :: State -> Either Error Client.Room.Lobby
forClientInLobby state =
  case state of
    WaitingForPlayers connectedPlayers ->
      Right
        $ Client.Room.Lobby
        $ ConnectionStates.ConnectionStates
          { ConnectionStates.freeSlots = freeSlots connectedPlayers,
            ConnectionStates.connectedPlayers = connectedPlayers
          }
    Started _ ->
      Left (InvalidMove "The game has already started")
    Paused missingPlayers gameState ->
      Right $
        Client.Room.Reconnecting (missingPlayersInfo missingPlayers gameState)

forClientInTheRoom :: Color -> State -> Client.Room.Room
forClientInTheRoom color state =
  case state of
    WaitingForPlayers connectedPlayers ->
      let connectionStates =
            ConnectionStates.ConnectionStates
              { ConnectionStates.freeSlots = freeSlots connectedPlayers,
                ConnectionStates.connectedPlayers = connectedPlayers
              }
       in case checkReady connectedPlayers of
            Nothing ->
              Client.Room.WaitingForPlayers connectionStates
            Just _ ->
              Client.Room.ReadyToStart connectionStates
    Started gameState ->
      Client.Room.Started (Game.playerState color gameState)
    Paused missingPlayers gameState ->
      Client.Room.Paused (missingPlayersInfo missingPlayers gameState)

missingPlayersInfo :: [Color] -> Game.State -> [(Color, Text)]
missingPlayersInfo missingPlayers gameState =
  foldl
    ( \result c ->
        case Game.playerName c gameState of
          Nothing -> result
          Just name -> (c, name) : result
    )
    []
    missingPlayers

checkReady :: [(Color, Text)] -> Maybe (TurnList (Color, Text))
checkReady connectedPlayers =
  case reverse connectedPlayers of
    first : second : rest ->
      Just (TurnList.init first (second : rest))
    _ ->
      Nothing

startGame :: State -> Either Error State
startGame state =
  case state of
    WaitingForPlayers connectedPlayers ->
      case checkReady connectedPlayers of
        Nothing ->
          Left (InvalidMove "Not enough players have joined the game yet")
        Just turnList ->
          Right (Started (Game.init turnList))
    Started _ ->
      Left (InvalidMove "Trying to start a game that has already started")
    Paused _ _ ->
      Left (InvalidMove "Trying to start a game that has already started")

broadcastChanges :: Room -> STM ()
broadcastChanges room =
  writeTChan (broadcastChannel room) (state room)

updateGame :: (Game.State -> Either Error Game.State) -> State -> Either Error State
updateGame fn state =
  case state of
    WaitingForPlayers _ ->
      Left (InvalidMove "Trying to make a move on a game that hasn't started yet")
    Started gameState ->
      case fn gameState of
        Left err ->
          Left err
        Right gameState' ->
          Right (Started gameState')
    Paused _ _ ->
      Left (InvalidMove "Trying to make a move on a paused game")
