{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Channel
  ( Channel.init,
    update,
    DataForClient,
    ClientCommand,
    Event (..),
    Effect (..),
    State,
  )
where

import Client.ConnectionStates (ConnectionStates)
import qualified Client.Room
import Data.Text (Text)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import Game (Color, Country)
import qualified Game.Room as Room
import qualified Server.Serialization as Serialization

data State
  = WaitingToJoin
  | InsideRoom Color

data ClientCommand
  = JoinRoom Color Text
  | StartGame
  | PaintCountry Color Country

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''ClientCommand

data DataForClient
  = NewLobbyUpdate ConnectionStates
  | NewRoomUpdate Client.Room.Room

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''DataForClient

data Event
  = Received ClientCommand
  | Update Room.State

data Effect
  = NoEffect
  | SendToClient DataForClient

init :: State
init = WaitingToJoin

update :: Room.State -> State -> Event -> (State, Room.State, Effect)
update roomState state event =
  case event of
    Received (JoinRoom color name) ->
      case Room.join color name roomState of
        Left _ ->
          (state, roomState, NoEffect)
        Right roomState' ->
          (state, roomState', NoEffect)
    Received StartGame ->
      (state, roomState, NoEffect)
    Received (PaintCountry color country) ->
      (state, roomState, NoEffect)
    Update _ ->
      -- TODO: build client notification and send it!
      (state, roomState, NoEffect)
