{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server (Server.run) where

import Data.Maybe (fromMaybe)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, IPAddrSource (..), LogType' (..), apacheLogger, initLogger)
import Servant
import Server.State (State)
import qualified Server.State as State
import qualified Server.WebSocket as WebSocket
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath.Posix as FilePath
import qualified System.Log.FastLogger.Date as Date
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssUseHash)

type StaticContentRoutes =
  "_build" :> Raw
    :<|> Raw

type Routes =
  WebSocket.WebSocketApi
    :<|> StaticContentRoutes

run :: IO ()
run = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  logFile <- lookupEnv "REQUESTS_LOG"
  logger <- initializeLogger (fromMaybe "./requests.log" logFile)
  state <- State.initIO
  let settings =
        setPort port
          $ setLogger logger
          $ defaultSettings
  putStrLn ("Starting the application at port " ++ show port)
  runSettings settings (app state)

initializeLogger :: String -> IO ApacheLogger
initializeLogger logFile = do
  let directory = FilePath.takeDirectory logFile
  Directory.createDirectoryIfMissing True directory
  getTime <- Date.newTimeCache Date.simpleTimeFormat
  apacheLogger
    <$> ( initLogger
            FromFallback
            (LogFileNoRotate logFile 4096)
            getTime
        )

app :: State -> Application
app state =
  serve api $
    WebSocket.server (State.roomVar state)
      :<|> staticContentServer

api :: Proxy Routes
api = Proxy

staticContentServer :: Server StaticContentRoutes
staticContentServer =
  serveDirectoryWebApp "ui/_build"
    :<|> (serveDirectoryWith ((defaultFileServerSettings "ui/static") {ssUseHash = True}))
