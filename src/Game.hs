{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game
  ( State,
    Country,
    Player (..),
    Game.init,
    join,
    blockedOn,
    otherPlayer,
    paintCountry,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import Game.Country (Country)
import qualified Server.Serialization as Serialization

data State
  = WaitingForRed
  | WaitingForBlue
  | Started
      { turn :: Player,
        -- NOTE: servant-elm generates buggy decoders for maps
        paintedCountries :: [(Country, Player)]
      }

data Player
  = Red
  | Blue
  deriving (Eq)

otherPlayer :: Player -> Player
otherPlayer player =
  case player of
    Red -> Blue
    Blue -> Red

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Player
deriveBoth defaultOptions ''State

init :: State
init =
  WaitingForRed

join :: State -> State
join state =
  case state of
    WaitingForRed -> WaitingForBlue
    WaitingForBlue ->
      Started
        { turn = Red,
          paintedCountries = []
        }
    Started _ _ ->
      -- TODO: signal error
      state

blockedOn :: State -> Player
blockedOn state =
  case state of
    WaitingForRed -> Red
    WaitingForBlue -> Blue
    Started _ _ -> turn state

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
