module Server.State
  ( State (..),
    Server.State.init,
    initIO,
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar (newTVar)
import Game.Room (Room)
import qualified Game.Room as Room

data State = State {roomVar :: TVar Room}

initIO :: IO State
initIO = atomically Server.State.init

init :: STM State
init = do
  roomVar <- Room.init >>= newTVar
  pure (State roomVar)
