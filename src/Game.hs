module Game
  ( State,
    Country,
    Color,
    Game.init,
    paintCountry,
    playerState,
    playerName,
  )
where

import qualified Client.Game
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Game.Color (Color)
import Game.Country (Country)
import Game.TurnList (TurnList)
import qualified Game.TurnList as TurnList
import Result (Error (..))

type Name = Text

data State = State
  { players :: TurnList (Color, Name),
    paintedCountries :: Map Country Color
  }
  deriving (Eq)

init :: TurnList (Color, Name) -> State
init players =
  State
    { players = players,
      paintedCountries = Map.empty
    }

playerName :: Color -> State -> Maybe Text
playerName color state =
  fmap snd (TurnList.find (\(c, _) -> color == c) (players state))

playerState :: Color -> State -> Client.Game.Game
playerState color state =
  Client.Game.Game
    { Client.Game.identity = color,
      -- NOTE: servant-elm generates buggy decoders for maps
      Client.Game.paintedCountries = Map.toList (paintedCountries state),
      Client.Game.instructions =
        if fst (TurnList.current (players state)) == color
          then Client.Game.PickCountryToPaint
          else Client.Game.WaitForOtherPlayer
    }

paintCountry :: Color -> Country -> State -> Either Error State
paintCountry color country state =
  if color == fst (TurnList.current (players state))
    then
      Right $
        State
          { players = TurnList.advance (players state),
            paintedCountries = Map.insert country color (paintedCountries state)
          }
    else Left (InvalidMove "Trying to make a move outside of the user's turn")
