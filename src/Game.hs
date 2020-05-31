{-# LANGUAGE OverloadedStrings #-}

module Game
  ( State,
    Country,
    Player (..),
    Error (..),
    LocalState,
    Instructions,
    Game.init,
    join,
    paintCountry,
    playerState,
  )
where

import qualified Data.Map as Map
import Data.Text (Text)
import Game.Country (Country (..))
import Game.LocalState (Instructions (..), LocalState (LocalState))
import qualified Game.LocalState as LocalState
import Game.Player (Player (..))

data State
  = WaitingForRed
  | WaitingForBlue
  | Started
      { turn :: Player,
        -- NOTE: servant-elm generates buggy decoders for maps
        paintedCountries :: [(Country, Player)]
      }

data Error
  = InvalidMove Text
  | InternalError Text

init :: State
init =
  WaitingForRed

join :: State -> Either Error (Player, State)
join state =
  case state of
    WaitingForRed ->
      let newState = WaitingForBlue
       in Right
            (Red, newState)
    WaitingForBlue ->
      let newState =
            Started
              { turn = Red,
                paintedCountries = []
              }
       in Right
            (Blue, newState)
    Started _ _ ->
      Left (InvalidMove "Trying to join a game that has already started")

playerState :: Player -> State -> LocalState
playerState player state =
  case state of
    WaitingForRed ->
      LocalState
        { LocalState.identity = player,
          LocalState.paintedCountries = [],
          LocalState.instructions = Wait
        }
    WaitingForBlue ->
      LocalState
        { LocalState.identity = player,
          LocalState.paintedCountries = [],
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

paintCountry :: Player -> Country -> State -> Either Error State
paintCountry player country state =
  case state of
    WaitingForRed ->
      Left (InvalidMove "Trying to paint a country on a game that hasn't started yet")
    WaitingForBlue ->
      Left (InvalidMove "Trying to paint a country on a game that hasn't started yet")
    Started _ _ ->
      if player == turn state
        then
          Right $
            Started
              { turn = otherPlayer (turn state),
                paintedCountries = Map.toList (Map.insert country player (Map.fromList (paintedCountries state)))
              }
        else Left (InvalidMove "Trying to make a move outside of the user's turn")

otherPlayer :: Player -> Player
otherPlayer player =
  case player of
    Red -> Blue
    Blue -> Red
