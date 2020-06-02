{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Game (Game (..), Instructions (..)) where

import Elm.Derive (defaultOptions, deriveBoth)
import Game.Country (Country)
import Game.Player (Player)

data Game = Game
  { identity :: Player,
    paintedCountries :: [(Country, Player)],
    instructions :: Instructions
  }

data Instructions
  = Wait
  | PaintCountry

deriveBoth defaultOptions ''Game
deriveBoth defaultOptions ''Instructions
