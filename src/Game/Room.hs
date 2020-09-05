module Game.Room
  ( Room,
    ClientChannel,
    State,
    Game.Room.init,
    connectionStates,
    subscribe,
    join,
    startGame,
    broadcastChanges,
    clientState,
    updateGame,
    playerConnected,
    state,
  )
where

import Client.ConnectionStates (ConnectionStates (..))
import qualified Client.Room
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, writeTChan)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Game
import Game.Color (Color)
import qualified Game.Color as Color
import qualified Game.TurnList as TurnList
import Result (Error (..), Result (..))

-- TODO: if a client disconnects, move to a new PlayersLeft state or similar

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers (Map Color ConnectionState)
  | Started (Map Color ClientChannel) Game.State

data ConnectionState
  = -- we are waiting for someone to join as this player
    Waiting
  | -- the slot is taken and we are waiting for the client to open the websocket
    -- connection
    Connecting Text
  | -- we have an active connection to the player
    Connected Text ClientChannel

type ClientChannel = TChan State

init :: STM Room
init = do
  broadcastChannel <- newBroadcastTChan
  pure
    Room
      { broadcastChannel = broadcastChannel,
        state = WaitingForPlayers (Map.empty)
      }

connectionStates :: State -> ConnectionStates
connectionStates state =
  case state of
    WaitingForPlayers connectionStates ->
      ConnectionStates
        { connectedPlayers = Map.keys connectionStates,
          freeSlots =
            filter
              ( \p -> case getConnectionState p connectionStates of
                  Waiting -> True
                  _ -> False
              )
              Color.all
        }
    Started playerChannels _ ->
      ConnectionStates
        { connectedPlayers = Map.keys playerChannels,
          freeSlots = []
        }

subscribe :: Room -> STM (TChan State)
subscribe room =
  dupTChan (broadcastChannel room)

getConnectionState :: Color -> Map Color ConnectionState -> ConnectionState
getConnectionState =
  Map.findWithDefault Waiting

join :: Color -> Text -> Room -> Result Room ()
join color name room =
  case state room of
    WaitingForPlayers connectionStates ->
      case getConnectionState color connectionStates of
        Waiting ->
          ( Right (),
            room {state = WaitingForPlayers (Map.insert color (Connecting name) connectionStates)}
          )
        _ ->
          ( Left (InvalidMove "Trying to join a game but all slots are taken"),
            room
          )
    Started _ gameState ->
      ( Left (InvalidMove "Trying to join a game that has already started"),
        room
      )

startGame :: Room -> Result Room ()
startGame room =
  case state room of
    WaitingForPlayers connectionStates ->
      case checkReady connectionStates of
        Nothing ->
          (Left (InvalidMove "Still waiting for players to join or connect"), room)
        Just playerChannels ->
          case Map.toList playerChannels of
            [] ->
              -- NOTE: this shouldn't happen!
              (Left (InvalidMove "Still waiting for players to join or connect"), room)
            (color0, (name0, _)) : otherPlayers ->
              let turnList =
                    ( TurnList.init
                        (color0, name0)
                        (map (\(color, (name, _)) -> (color, name)) otherPlayers)
                    )
               in ( Right (),
                    room
                      { state =
                          Started
                            (Map.map snd playerChannels)
                            (Game.init turnList)
                      }
                  )
    Started _ _ ->
      ( Left (InvalidMove "Trying to start a game that has already started"),
        room
      )

playerConnected :: Color -> Room -> STM (Maybe ClientChannel, Room)
playerConnected player room =
  case state room of
    WaitingForPlayers connectionStates ->
      case getConnectionState player connectionStates of
        Connecting name ->
          do
            playerChannel <- dupTChan (broadcastChannel room)
            let connectionStates_ = Map.insert player (Connected name playerChannel) connectionStates
            pure
              ( Just playerChannel,
                room {state = WaitingForPlayers connectionStates_}
              )
        _ ->
          pure (Nothing, room)
    Started _ _ ->
      pure (Nothing, room)

checkReady :: Map Color ConnectionState -> Maybe (Map Color (Text, ClientChannel))
checkReady connectionStates =
  let playerChannels =
        foldr
          ( \player result ->
              case getConnectionState player connectionStates of
                Connected name channel -> Map.insert player (name, channel) result
                _ -> result
          )
          (Map.empty)
          (Map.keys connectionStates)
   in if length playerChannels >= 2
        then Just playerChannels
        else Nothing

broadcastChanges :: Room -> STM ()
broadcastChanges room =
  writeTChan (broadcastChannel room) (state room)

clientState :: Color -> State -> Client.Room.Room
clientState player state =
  case state of
    WaitingForPlayers connections ->
      case checkReady connections of
        Nothing ->
          Client.Room.WaitingForPlayers (connectionStates state)
        Just _ ->
          Client.Room.ReadyToStart (connectionStates state)
    Started _ gameState ->
      Client.Room.Started (Game.playerState player gameState)

updateGame :: (Game.State -> Result Game.State result) -> Room -> Result Room result
updateGame fn room =
  case state room of
    WaitingForPlayers _ ->
      ( Left (InvalidMove "Trying to make a move on a game that hasn't started yet"),
        room
      )
    Started connections gameState ->
      let (result, gameState') = fn gameState
          state' = Started connections gameState'
       in case result of
            Left err ->
              ( Left err,
                room {state = state'}
              )
            Right val ->
              ( Right val,
                room {state = state'}
              )
