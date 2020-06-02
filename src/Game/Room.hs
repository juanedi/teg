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
import Result (Error (..), Result (..))

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
      Result
        { response = Right Red,
          newState = WaitingForBlue
        }
    WaitingForBlue ->
      Result
        { response = Right Blue,
          newState = Started Game.init
        }
    Started _ ->
      Result
        { response = Left (InvalidMove "Trying to join a game that has already started"),
          newState = room
        }

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
      Result
        { response = Left (InvalidMove "Trying to make a move on a game that hasn't started yet"),
          newState = room
        }
    WaitingForBlue ->
      Result
        { response = Left (InvalidMove "Trying to make a move on a game that hasn't started yet"),
          newState = room
        }
    Started gameState ->
      let result = fn gameState
          room' = Started (Result.newState result)
       in case Result.response result of
            Left err ->
              Result
                { response = Left err,
                  newState = room'
                }
            Right val ->
              Result
                { response = Right val,
                  newState = room'
                }
