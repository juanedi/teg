module Server.State
  ( State,
    Server.State.init,
    update_,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Game

type State = TVar Game.State

init :: IO (State)
init = TVar.newTVarIO (Game.init)

update_ :: (Game.State -> (a, Game.State)) -> State -> IO a
update_ fn state = do
  STM.atomically (STM.stateTVar state fn)
