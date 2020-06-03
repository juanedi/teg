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
import Result (Result)
import qualified Result

data State = State {roomVar :: TVar Room}

initIO :: IO State
initIO = atomically Server.State.init

init :: STM State
init = do
  roomVar <- Room.init >>= newTVar
  pure (State roomVar)

updateRoom :: (Room -> Result Room val) -> State -> STM (Result Room val)
updateRoom fn (State roomVar) = do
  room <- readTVar roomVar
  let result = fn room
  let room' = Result.newState result
  writeTVar roomVar room'
  Room.broadcastChanges room'
  pure result

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . atomically
