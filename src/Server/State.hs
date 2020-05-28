module Server.State
  ( State (..),
    Server.State.init,
  )
where

import qualified Game

data State = State
  { gameState :: Game.State
  }

init :: State
init =
  State
    { gameState = Game.init
    }
