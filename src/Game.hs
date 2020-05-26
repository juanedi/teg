{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game
  ( State,
    Country,
    Player (..),
    Game.init,
    paintCountry,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
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

nextPlayer :: Player -> Player
nextPlayer player =
  case player of
    Red -> Blue
    Blue -> Red

data Country
  = Alaska
  | Arabia
  | Aral
  | Argentina
  | Australia
  | Borneo
  | Brasil
  | California
  | Canada
  | Chile
  | China
  | Colombia
  | Egypt
  | Ethiopia
  | France
  | Germany
  | Gobi
  | GreatBritain
  | Greenland
  | Iceland
  | India
  | Iran
  | Israel
  | Italy
  | Japan
  | Java
  | Kamchatka
  | Labrador
  | Madagascar
  | Malasya
  | Mexico
  | Mongolia
  | NewYork
  | Oregon
  | Peru
  | Poland
  | Russia
  | Sahara
  | SouthAfrica
  | Spain
  | Sumatra
  | Sweden
  | Syberia
  | Tartary
  | Taymir
  | Terranova
  | Turkey
  | Uruguay
  | Yukon
  | Zaire
  deriving (Eq, Ord, Show, Read)

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Country
deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Player
deriveBoth defaultOptions ''State

init :: State
init =
  WaitingForRed

-- State
--   { turn = Red,
--     paintedCountries = []
--   }

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
            { turn = nextPlayer (turn state),
              paintedCountries = Map.toList (Map.insert country player (Map.fromList (paintedCountries state)))
            }
        else-- TODO: signal error
          state
