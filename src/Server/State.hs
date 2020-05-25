module Server.State
  ( State,
    Server.State.init,
    Server.State.read,
    update,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Game

type State = TVar Game.State

init :: IO (State)
init = TVar.newTVarIO (Game.init)

read :: State -> IO (Game.State)
read state =
  TVar.readTVarIO state

update :: (Game.State -> Game.State) -> State -> IO (Game.State)
update fn state = do
  STM.atomically (TVar.modifyTVar state fn)
  Server.State.read state
