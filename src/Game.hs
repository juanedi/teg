module Game
  ( State,
    Country,
    Color (..),
    Game.init,
    paintCountry,
    playerState,
  )
where

import qualified Client.Game
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Game.Country (Country (..))
import Game.Color (Color (..))
import Game.TurnList (TurnList)
import qualified Game.TurnList as TurnList
import Result (Error (..))

data State = State
  { players :: TurnList Color,
    paintedCountries :: Map Country Color
  }

init :: TurnList Color -> State
init players =
  State
    { players = players,
      paintedCountries = Map.empty
    }

playerState :: Color -> State -> Client.Game.Game
playerState player state =
  Client.Game.Game
    { Client.Game.identity = player,
      -- NOTE: servant-elm generates buggy decoders for maps
      Client.Game.paintedCountries = Map.toList (paintedCountries state),
      Client.Game.instructions =
        if TurnList.current (players state) == player
          then Client.Game.PaintCountry
          else Client.Game.Wait
    }

paintCountry :: Color -> Country -> State -> Either Error State
paintCountry player country state =
  if player == TurnList.current (players state)
    then
      Right $
        State
          { players = TurnList.advance (players state),
            paintedCountries = Map.insert country player (paintedCountries state)
          }
    else Left (InvalidMove "Trying to make a move outside of the user's turn")
