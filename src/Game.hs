{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game
  ( State,
    Country,
    new,
    tagToApiLabel,
  )
where

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

data State = State
  { lastClickedCountry :: Maybe Country,
    hoveredCountry :: Maybe Country
  }

new :: State
new =
  State
    { lastClickedCountry = Just Argentina,
      hoveredCountry = Nothing
    }

tagToApiLabel :: String -> String
tagToApiLabel constructorTag =
  Casing.toQuietSnake (Casing.fromHumps constructorTag)
