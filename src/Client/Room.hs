{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Room (Room (..)) where

import qualified Client.Game
import Elm.Derive (defaultOptions, deriveBoth)

data Room
  = WaitingForPlayers
  | Started Client.Game.Game

deriveBoth defaultOptions ''Room
