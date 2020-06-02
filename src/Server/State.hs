module Server.State
  ( State,
    Server.State.init,
    readRoom,
    updateRoom,
    playerUpdatesChannel,
    runSTM,
  )
where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Game.Room (Room)
import qualified Game.Room as Room

data State = State
  { room :: TVar Room,
    broadcastChannel :: TChan Room
  }

init :: IO State
init = do
  room <- newTVarIO Room.init
  broadcastChannel <- newBroadcastTChanIO
  pure
    ( State
        { room = room,
          broadcastChannel = broadcastChannel
        }
    )

readRoom :: State -> STM Room
readRoom state =
  readTVar (room state)

updateRoom :: Room -> State -> STM ()
updateRoom room_ state = do
  writeTVar (room state) room_
  writeTChan (broadcastChannel state) room_

playerUpdatesChannel :: State -> STM (TChan Room)
playerUpdatesChannel state =
  dupTChan (broadcastChannel state)

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . STM.atomically
