module Game.Room
  ( Room,
    State,
    Game.Room.init,
    subscribe,
    join,
    startGame,
    broadcastChanges,
    updateGame,
    state,
  )
where

import Client.ConnectionStates (ConnectionStates (..))
import qualified Client.Room
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, writeTChan)
import Data.Text (Text)
import qualified Game
import Game.Color (Color)
import qualified Game.Color as Color
import qualified Game.TurnList as TurnList
import Result (Error (..), Result (..))

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers [Color] [(Color, Text)]
  | Started Game.State
  deriving (Eq)

init :: STM Room
init =
  do
    broadcastChannel <- newBroadcastTChan
    pure
      Room
        { broadcastChannel = broadcastChannel,
          state = WaitingForPlayers [] []
        }

subscribe :: Room -> STM (TChan State)
subscribe room =
  dupTChan (broadcastChannel room)

join :: Color -> Text -> State -> Either Error State
join color name state =
  case state of
    WaitingForPlayers freeSlots connectedPlayers ->
      if elem color freeSlots
        then Right state
        else Left (InvalidMove "That slot has already been taken")
    Started _ ->
      Left (InvalidMove "Trying to join a game that has already started")

forClient :: State -> ConnectionStates
forClient state =
  case state of
    WaitingForPlayers freeSlots connectedPlayers ->
      ConnectionStates
        { freeSlots = freeSlots,
          connectedPlayers = connectedPlayers
        }

startGame :: Room -> Result Room ()
startGame room =
  undefined

broadcastChanges :: Room -> STM ()
broadcastChanges room =
  writeTChan (broadcastChannel room) (state room)

updateGame :: (Game.State -> Result Game.State result) -> Room -> Result Room result
updateGame fn room =
  case state room of
    WaitingForPlayers _ _ ->
      ( Left (InvalidMove "Trying to make a move on a game that hasn't started yet"),
        room
      )
    Started gameState ->
      let (result, gameState') = fn gameState
          state' = Started gameState'
       in case result of
            Left err ->
              ( Left err,
                room {state = state'}
              )
            Right val ->
              ( Right val,
                room {state = state'}
              )
