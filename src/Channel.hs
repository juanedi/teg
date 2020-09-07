module Channel
  ( Channel.init,
    update,
    DataForClient,
    Event (..),
    Effect (..),
    State,
  )
where

import Data.Text (Text)
import Game (Color, Country)
import qualified Game.Room as Room

data State
  = WaitingToJoin
  | InsideRoom Color

-- data ClientCommand
--   = JoinRoom Color Text
--   | StartGame
--   | PaintCountry Color Country

type ClientCommand = Text

-- data DataForClient
--   = -- TODO: command responses would go here too
--     NewLobbyUpdate ConnectionStates
--   | NewRoomUpdate Client.Room.Room

type DataForClient = Text

data Event
  = Received ClientCommand
  | Update Room.State
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
    Received cmd ->
      (state, SendToClient cmd)
    Update _ ->
      (state, SendToClient "room update!")
    ConnectionClosed ->
      (state, HangUp)
