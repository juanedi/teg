{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.ConnectionStates (ConnectionStates (..)) where

import Elm.Derive (defaultOptions, deriveBoth)
import Game.Player (Player)

data ConnectionStates = ConnectionStates
  { connectedPlayers :: [Player],
    freeSlots :: [Player]
  }

deriveBoth defaultOptions ''ConnectionStates
