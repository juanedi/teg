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

init :: State
init = WaitingToJoin

update :: Room.State -> State -> ClientCommand -> (State, Room.State)
update roomState state cmd =
  case cmd of
    JoinRoom color name ->
      case Room.join color name roomState of
        Left _ ->
          (state, roomState)
        Right roomState' ->
          (state, roomState')
    StartGame ->
      (state, roomState)
    PaintCountry color country ->
      (state, roomState)
