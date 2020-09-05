{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Game (Game (..), Instructions (..)) where

import Elm.Derive (defaultOptions, deriveBoth)
import Game.Country (Country)
import Game.Color (Color)

data Game = Game
  { identity :: Color,
    paintedCountries :: [(Country, Color)],
    instructions :: Instructions
  }

data Instructions
  = Wait
  | PaintCountry

deriveBoth defaultOptions ''Game
deriveBoth defaultOptions ''Instructions
