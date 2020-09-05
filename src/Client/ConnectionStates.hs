{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.ConnectionStates (ConnectionStates (..)) where

import Data.Text (Text)
import Elm.Derive (defaultOptions, deriveBoth)
import Game.Color (Color)

data ConnectionStates = ConnectionStates
  { connectedPlayers :: [(Color, Text)],
    freeSlots :: [Color]
  }

deriveBoth defaultOptions ''ConnectionStates
