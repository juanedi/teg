{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Room (Room (..)) where

import qualified Client.Game
import Elm.Derive (defaultOptions, deriveBoth)
import Game.Player (Player)

data Room
  = WaitingForPlayers [Player]
  | Started Client.Game.Game

deriveBoth defaultOptions ''Room
