module Server.State
  ( State (..),
    Server.State.init,
  )
where

-- import qualified Control.Concurrent.STM as STM
-- import Control.Concurrent.STM.TVar (TVar)
-- import qualified Control.Concurrent.STM.TVar as TVar
import qualified Game

data State = State
  { gameState :: Game.State
  }

init :: State
init =
  State
    { gameState = Game.init
    }

-- update_ :: (Game.State -> Either Game.Error (a, Game.State)) -> State -> IO a
-- update_ fn state = do
--   STM.atomically (STM.stateTVar state fn)
