module Game.Room
  ( Room,
    ClientChannel,
    State,
    Game.Room.init,
    connectionStates,
    subscribe,
    join,
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
import qualified Game
import Game.Player (Player)
import qualified Game.Player as Player
import qualified Game.TurnList as TurnList
import Result (Error (..), Result (..))

-- TODO: if a client disconnects, move to a new PlayersLeft state or similar

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers (Map Player ConnectionState)
  | Started (Map Player ClientChannel) Game.State

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
              Player.all
        }
    Started playerChannels _ ->
      ConnectionStates
        { connectedPlayers = Map.keys playerChannels,
          freeSlots = []
        }

subscribe :: Room -> STM (TChan State)
subscribe room =
  dupTChan (broadcastChannel room)

getConnectionState :: Player -> Map Player ConnectionState -> ConnectionState
getConnectionState =
  Map.findWithDefault Waiting

join :: Player -> Room -> Result Room ()
join player room =
  case state room of
    WaitingForPlayers connectionStates ->
      case getConnectionState player connectionStates of
        Waiting ->
          ( Right (),
            room {state = WaitingForPlayers (Map.insert player Connecting connectionStates)}
          )
        _ ->
          ( Left (InvalidMove "Trying to join a game but all slots are taken"),
            room
          )
    Started _ gameState ->
      ( Left (InvalidMove "Trying to join a game that has already started"),
        room
      )

playerConnected :: Player -> Room -> STM (Maybe ClientChannel, Room)
playerConnected player room =
  case state room of
    WaitingForPlayers connectionStates ->
      case getConnectionState player connectionStates of
        Connecting ->
          do
            playerChannel <- dupTChan (broadcastChannel room)
            let connectionStates_ = Map.insert player (Connected playerChannel) connectionStates
            pure
              ( Just playerChannel,
                room
                  { state = case checkReady connectionStates_ of
                      Nothing ->
                        WaitingForPlayers connectionStates_
                      Just playerChannels ->
                        case Map.keys playerChannels of
                          [] ->
                            -- NOTE: this shouldn't happen!
                            WaitingForPlayers connectionStates_
                          firstPlayer : otherPlayers ->
                            Started playerChannels (Game.init (TurnList.init firstPlayer otherPlayers))
                  }
              )
        _ ->
          pure (Nothing, room)
    Started _ _ ->
      pure (Nothing, room)

checkReady :: Map Player ConnectionState -> Maybe (Map Player ClientChannel)
checkReady connectionStates =
  let -- TODO: for the moment we start the game as soon as there are two connected
      -- players change this so that we start when the minimum is reached and
      -- someone clicks a "start game" button.
      expectedPlayerCount = 2
      playerChannels =
        foldr
          ( \player result ->
              case getConnectionState player connectionStates of
                Connected channel -> Map.insert player channel result
                _ -> result
          )
          (Map.empty)
          (Map.keys connectionStates)
   in if length playerChannels >= expectedPlayerCount
        then Just playerChannels
        else Nothing

broadcastChanges :: Room -> STM ()
broadcastChanges room =
  writeTChan (broadcastChannel room) (state room)

clientState :: Player -> State -> Client.Room.Room
clientState player state =
  case state of
    WaitingForPlayers _ -> Client.Room.WaitingForPlayers (connectionStates state)
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
