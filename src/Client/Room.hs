{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Client.Room
  ( Room (..),
    Lobby (..),
  )
where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Game
import Data.Text (Text)
import Elm.Derive (defaultOptions, deriveBoth)
import Game.Color (Color)

data Lobby
  = Lobby ConnectionStates
  | Reconnecting [(Color, Text)]

deriveBoth defaultOptions ''Lobby

data Room
  = WaitingForPlayers ConnectionStates
  | ReadyToStart ConnectionStates
  | Started Client.Game.Game
  | Paused [(Color, Text)]

deriveBoth defaultOptions ''Room
