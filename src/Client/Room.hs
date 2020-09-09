{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Room (Room (..), Lobby (..)) where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Game
import Elm.Derive (defaultOptions, deriveBoth)

data Lobby
  = Lobby ConnectionStates

deriveBoth defaultOptions ''Lobby

data Room
  = WaitingForPlayers ConnectionStates
  | ReadyToStart ConnectionStates
  | Started Client.Game.Game

deriveBoth defaultOptions ''Room
