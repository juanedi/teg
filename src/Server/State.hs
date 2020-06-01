module Server.State
  ( State,
    Server.State.init,
    readGameState,
    updateGameState,
    playerUpdatesChannel,
    runSTM,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Game

data State = State
  { -- TODO: move all session management state here, so that we can keep track
    -- of open websocket channels.
    gameState :: TVar Game.State,
    broadcastChannel :: TChan Game.State
  }

init :: IO State
init = do
  gameState <- newTVarIO Game.init
  broadcastChannel <- newBroadcastTChanIO
  pure
    ( State
        { gameState = gameState,
          broadcastChannel = broadcastChannel
        }
    )

readGameState :: State -> STM Game.State
readGameState state =
  readTVar (gameState state)

updateGameState :: Game.State -> State -> STM ()
updateGameState gs state = do
  writeTVar (gameState state) gs
  writeTChan (broadcastChannel state) gs

playerUpdatesChannel :: State -> STM (TChan Game.State)
playerUpdatesChannel state =
  dupTChan (broadcastChannel state)

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . STM.atomically
