{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.FullDuplexWebSocket
  ( WebSocketApi,
    server,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Text (pack)
import Network.WebSockets.Connection (PendingConnection, acceptRequest, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket

type WebSocketApi = "stream" :> WebSocketPending

server :: Server WebSocketApi
server = streamData
  where
    streamData :: MonadIO m => PendingConnection -> m ()
    streamData pc = do
      c <- liftIO $ acceptRequest pc
      liftIO
        $ withPingThread c 10 (return ())
        $ liftIO . forM_ [1 ..]
        $ \i ->
          sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
