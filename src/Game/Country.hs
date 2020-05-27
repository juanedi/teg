{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Country (Country) where

import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Server.Serialization as Serialization

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
