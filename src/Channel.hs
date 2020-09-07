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

-- data DataForClient
--   = -- TODO: command responses would go here too
--     NewLobbyUpdate ConnectionStates
--   | NewRoomUpdate Client.Room.Room

type DataForClient = Text

data Event
  = Received ClientCommand
  | Update Room.State
  | ReceivedInvalidMessage Text
  | ConnectionClosed

data Effect
  = NoEffect
  | SendToClient DataForClient
  | HangUp
  deriving (Show)

init :: State
init = WaitingToJoin

update :: State -> Event -> (State, Effect)
update state event =
  case event of
    Received (JoinRoom color name) ->
      (state, SendToClient "ACK join room")
    Received StartGame ->
      (state, SendToClient "ACK start game")
    Received (PaintCountry color country) ->
      (state, SendToClient "ACK paint country")
    Update _ ->
      (state, SendToClient "room update!")
    ReceivedInvalidMessage err ->
      (state, SendToClient "invalid message!")
    ConnectionClosed ->
      (state, HangUp)
