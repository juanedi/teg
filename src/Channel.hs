{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Channel
  ( Channel.init,
    roomNotification,
    update,
    DataForClient (..),
    ClientCommand,
    State,
  )
where

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
  = LobbyUpdate Client.Room.Lobby
  | RoomUpdate Client.Room.Room

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''DataForClient

init :: State
init = WaitingToJoin

roomNotification :: Room.State -> State -> Maybe DataForClient
roomNotification roomState channelState =
  case channelState of
    WaitingToJoin ->
      case Room.forClientInLobby roomState of
        Left error ->
          Nothing
        Right lobby ->
          Just (LobbyUpdate lobby)
    InsideRoom color ->
      Just (RoomUpdate (Room.forClientInTheRoom color roomState))

update :: Room.State -> State -> ClientCommand -> (State, Room.State)
update roomState state cmd =
  case cmd of
    JoinRoom color name ->
      case Room.join color name roomState of
        Left _ ->
          (state, roomState)
        Right roomState' ->
          (InsideRoom color, roomState')
    StartGame ->
      (state, roomState)
    PaintCountry color country ->
      (state, roomState)
