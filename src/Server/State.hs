module Server.State
  ( State (..),
  )
where

import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar (TVar)
import qualified Game

data State = State
  { gameState :: TVar Game.State,
    broadcastChannel :: TChan Game.State
  }
