module Game
  ( State,
    Country,
    Player (..),
    Game.init,
    paintCountry,
    playerState,
  )
where

import qualified Client.Game
import qualified Data.Map as Map
import Game.Country (Country (..))
import Game.Player (Player (..))
import Result (Error (..))

data State = State
  { turn :: Player,
    -- NOTE: servant-elm generates buggy decoders for maps
    paintedCountries :: [(Country, Player)]
  }

init :: State
init =
  State
    { turn = Red,
      paintedCountries = []
    }

playerState :: Player -> State -> Client.Game.Game
playerState player state =
  Client.Game.Game
    { Client.Game.identity = player,
      Client.Game.paintedCountries = paintedCountries state,
      Client.Game.instructions =
        if turn state == player
          then Client.Game.PaintCountry
          else Client.Game.Wait
    }

paintCountry :: Player -> Country -> State -> Either Error State
paintCountry player country state =
  if player == turn state
    then
      Right $
        State
          { turn = otherPlayer (turn state),
            paintedCountries = Map.toList (Map.insert country player (Map.fromList (paintedCountries state)))
          }
    else Left (InvalidMove "Trying to make a move outside of the user's turn")

otherPlayer :: Player -> Player
otherPlayer player =
  case player of
    Red -> Blue
    Blue -> Red
