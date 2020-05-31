{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.LocalState (LocalState (..), Instructions (..)) where

import Elm.Derive (defaultOptions, deriveBoth)
import Game.Country (Country)
import Game.Player (Player)

data LocalState = LocalState
  { identity :: Player,
    paintedCountries :: [(Country, Player)],
    instructions :: Instructions
  }

data Instructions
  = Wait
  | PaintCountry

deriveBoth defaultOptions ''LocalState
deriveBoth defaultOptions ''Instructions
