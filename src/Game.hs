module Game
  ( State,
    Country,
    Player (..),
    LocalState,
    Instructions,
    Game.init,
    join,
    otherPlayer,
    playerState,
    paintCountry,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Game.Country (Country (..))
import Game.LocalState (Instructions (..), LocalState (LocalState))
import qualified Game.LocalState as LocalState
import Game.Player (Player (..))
import qualified Server.Serialization as Serialization

data State
  = WaitingForRed
  | WaitingForBlue
  | Started
      { turn :: Player,
        -- NOTE: servant-elm generates buggy decoders for maps
        paintedCountries :: [(Country, Player)]
      }

init :: State
init =
  WaitingForRed

join :: State -> (Player, State)
join state =
  case state of
    WaitingForRed -> (Red, WaitingForBlue)
    WaitingForBlue ->
      ( Blue,
        Started
          { turn = Red,
            paintedCountries = []
          }
      )
    Started _ _ ->
      -- TODO: signal error
      (Red, WaitingForBlue)

playerState :: Player -> State -> LocalState
playerState player state =
  case state of
    WaitingForRed ->
      LocalState
        { LocalState.identity = player,
          LocalState.paintedCountries = paintedCountries state,
          LocalState.instructions = Wait
        }
    WaitingForBlue ->
      LocalState
        { LocalState.identity = player,
          LocalState.paintedCountries = paintedCountries state,
          LocalState.instructions = Wait
        }
    Started _ _ ->
      LocalState
        { LocalState.identity = player,
          LocalState.paintedCountries = paintedCountries state,
          LocalState.instructions =
            if turn state == player
              then PaintCountry
              else Wait
        }

paintCountry :: Player -> Country -> State -> State
paintCountry player country state =
  case state of
    WaitingForRed ->
      -- TODO: signal error
      state
    WaitingForBlue ->
      -- TODO: signal error
      state
    Started _ _ ->
      if player == turn state
        then
          Started
            { turn = otherPlayer (turn state),
              paintedCountries = Map.toList (Map.insert country player (Map.fromList (paintedCountries state)))
            }
        else-- TODO: signal error
          state

otherPlayer :: Player -> Player
otherPlayer player =
  case player of
    Red -> Blue
    Blue -> Red
