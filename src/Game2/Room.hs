module Game2.Room where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan, writeTChan)
import qualified Game
import Game.Color (Color)

data Room = Room
  { broadcastChannel :: TChan State,
    state :: State
  }

data State
  = WaitingForPlayers [Color]
  | Started Game.State

init :: STM Room
init =
  do
    broadcastChannel <- newBroadcastTChan
    pure
      Room
        { broadcastChannel = broadcastChannel,
          state = WaitingForPlayers []
        }
