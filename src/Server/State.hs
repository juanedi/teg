module Server.State
  ( State (..),
    Server.State.init,
    initIO,
    updateRoom,
    runSTM,
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Game.Room (Room)
import qualified Game.Room as Room

data State = State {roomVar :: TVar Room}

initIO :: IO State
initIO = atomically Server.State.init

init :: STM State
init = do
  roomVar <- Room.init >>= newTVar
  pure (State roomVar)

updateRoom :: (Room.State -> STM (val, Room.State)) -> State -> STM val
updateRoom fn (State roomVar) = do
  room <- readTVar roomVar
  let initialRoomState = Room.state room
  (val, newRoomState) <- fn initialRoomState
  let room' = room {Room.state = newRoomState}
  writeTVar roomVar room'
  -- TODO: the initial update is not being sent because there was no change make
  -- it so that we don't rely on equality checks but also don't enter an
  -- infinite loop when a notification is processed.
  if initialRoomState /= newRoomState
    then Room.broadcastChanges room'
    else return ()
  pure val

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . atomically
