module Game.Room
  ( Room,
    Game.Room.init,
    join,
    clientState,
    updateGame,
  )
where

import qualified Client.Room
import qualified Game
import Game.Player (Player (..))
import qualified Result
import Result (Error (..), Result)

data Room
  = WaitingForRed
  | WaitingForBlue
  | Started Game.State

init :: Room
init = WaitingForRed

join :: Room -> Result Room Player
join room =
  case room of
    WaitingForRed ->
      Result.succeed
        WaitingForBlue
        Red
    WaitingForBlue ->
      Result.succeed
        (Started Game.init)
        Blue
    Started _ ->
      Result.err
        room
        (InvalidMove "Trying to join a game that has already started")

clientState :: Player -> Room -> Client.Room.Room
clientState player room =
  case room of
    WaitingForRed -> Client.Room.WaitingForPlayers
    WaitingForBlue -> Client.Room.WaitingForPlayers
    Started gameState ->
      Client.Room.Started (Game.playerState player gameState)

updateGame :: (Game.State -> Result Game.State result) -> Room -> Result Room result
updateGame fn room =
  case room of
    WaitingForRed ->
      Result.err
        room
        (InvalidMove "Trying to make a move on a game that hasn't started yet")
    WaitingForBlue ->
      Result.err
        room
        (InvalidMove "Trying to make a move on a game that hasn't started yet")
    Started gameState ->
      let result = fn gameState
          room' = Started (Result.newState result)
       in case Result.response result of
            Left err ->
              Result.err room' err
            Right val ->
              Result.succeed room' val
