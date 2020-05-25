{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game
  ( State,
    Country,
    Game.init,
    paintCountry,
    tagToApiLabel,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Casing as Casing

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
  deriving (Eq, Ord)

data State = State
  { paintedCountries :: Set Country
  }

init :: State
init =
  State
    { paintedCountries = Set.empty
    }

paintCountry :: Country -> State -> State
paintCountry country state =
  State
    { paintedCountries = Set.insert country (paintedCountries state)
    }

tagToApiLabel :: String -> String
tagToApiLabel constructorTag =
  Casing.toQuietSnake (Casing.fromHumps constructorTag)
