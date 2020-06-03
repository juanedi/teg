module Game.Room
  ( Room,
    ClientChannel,
    State,
    Game.Room.init,
    join,
    broadcastChanges,
    clientState,
    updateGame,
    playerConnected,
    state,
  )
where

import qualified Client.Room
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, writeTChan)
import qualified Game
import Game.Player (Player (..))
import Result (Error (..), Result (..))

-- TODO: if a client disconnects, move to a new PlayersLeft state or similar

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers (ConnectionState, ConnectionState)
  | Started (ClientChannel, ClientChannel) Game.State

data ConnectionState
  = -- we are waiting for someone to join as this player
    Waiting
  | -- the slot is taken and we are waiting for the client to open the websocket
    -- connection
    Connecting
  | -- we have an active connection to the player
    Connected ClientChannel

type ClientChannel = TChan State

init :: STM Room
init = do
  broadcastChannel <- newBroadcastTChan
  pure
    Room
      { broadcastChannel = broadcastChannel,
        state = WaitingForPlayers (Waiting, Waiting)
      }

join :: Room -> Result Room Player
join room =
  case state room of
    WaitingForPlayers (Waiting, Waiting) ->
      Result
        { response = Right Red,
          newState = room {state = WaitingForPlayers (Connecting, Waiting)}
        }
    WaitingForPlayers (redPlayerState, Waiting) ->
      Result
        { response = Right Blue,
          newState = room {state = WaitingForPlayers (redPlayerState, Connecting)}
        }
    WaitingForPlayers _ ->
      Result
        { response = Left (InvalidMove "Trying to join a game but all slots are taken"),
          newState = room
        }
    Started _ gameState ->
      Result
        { response = Left (InvalidMove "Trying to join a game that has already started"),
          newState = room
        }

playerConnected :: Player -> Room -> STM (Room, Maybe ClientChannel)
playerConnected player room =
  case (player, state room) of
    (Red, WaitingForPlayers (Connecting, blueState)) ->
      do
        playerChannel <- dupTChan (broadcastChannel room)
        pure
          ( room {state = startIfReady $ WaitingForPlayers (Connected playerChannel, blueState)},
            Just playerChannel
          )
    (Blue, WaitingForPlayers (redState, Connecting)) ->
      do
        playerChannel <- dupTChan (broadcastChannel room)
        pure
          ( room {state = startIfReady $ WaitingForPlayers (redState, Connected playerChannel)},
            Just playerChannel
          )
    _ ->
      pure (room, Nothing)

startIfReady :: State -> State
startIfReady state =
  case state of
    WaitingForPlayers (Connected channel, Connected channel') ->
      Started (channel, channel') Game.init
    _ ->
      state

broadcastChanges :: Room -> STM ()
broadcastChanges room =
  writeTChan (broadcastChannel room) (state room)

clientState :: Player -> State -> Client.Room.Room
clientState player state =
  case state of
    WaitingForPlayers _ -> Client.Room.WaitingForPlayers
    Started _ gameState ->
      Client.Room.Started (Game.playerState player gameState)

updateGame :: (Game.State -> Result Game.State result) -> Room -> Result Room result
updateGame fn room =
  case state room of
    WaitingForPlayers _ ->
      Result
        { response = Left (InvalidMove "Trying to make a move on a game that hasn't started yet"),
          newState = room
        }
    Started connections gameState ->
      let result = fn gameState
          state' = Started connections (Result.newState result)
       in case Result.response result of
            Left err ->
              Result
                { response = Left err,
                  newState = room {state = state'}
                }
            Right val ->
              Result
                { response = Right val,
                  newState = room {state = state'}
                }
