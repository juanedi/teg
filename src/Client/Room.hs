{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Room (Room (..)) where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Game
import Elm.Derive (defaultOptions, deriveBoth)
import Game.Player (Player)

data Room
  = WaitingForPlayers ConnectionStates
  | Started Client.Game.Game

deriveBoth defaultOptions ''Room
