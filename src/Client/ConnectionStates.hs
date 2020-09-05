{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.ConnectionStates (ConnectionStates (..)) where

import Elm.Derive (defaultOptions, deriveBoth)
import Game.Color (Color)

data ConnectionStates = ConnectionStates
  { connectedPlayers :: [Color],
    freeSlots :: [Color]
  }

deriveBoth defaultOptions ''ConnectionStates
