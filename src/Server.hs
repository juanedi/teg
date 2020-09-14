{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server (Server.run) where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Data.Maybe (fromMaybe)
import Game.Room (Room)
import qualified Game.Room as Room
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, IPAddrSource (..), LogType' (..), apacheLogger, initLogger)
import Servant
import Servant.HTML.Blaze
import qualified Server.Flags as Flags
import qualified Server.Templates as Templates
import qualified Server.WebSocket as WebSocket
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath.Posix as FilePath
import qualified System.Log.FastLogger.Date as Date
import qualified Text.Blaze.Html5 as H
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssUseHash)

type StaticContentRoutes =
  "_build" :> Raw
    :<|> Raw

type Routes =
  ( "g"
      :> Capture "roomId" Room.Id
      :> ( Get '[HTML] H.Html
             :<|> "ws" :> WebSocket.WebSocketApi
         )
  )
    :<|> StaticContentRoutes

run :: IO ()
run = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  logFile <- lookupEnv "REQUESTS_LOG"
  logger <- initializeLogger (fromMaybe "./requests.log" logFile)
  roomVar <- STM.atomically (Room.init >>= newTVar)
  let settings =
        setPort port
          $ setLogger logger
          $ defaultSettings
  putStrLn ("Starting the application at port " ++ show port)
  runSettings settings (app roomVar)

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

app :: TVar Room -> Application
app roomVar =
  serve api $
    ( \(Room.Id roomId) ->
        ( return
            ( Templates.game
                ( Flags.Flags
                    { Flags.boardSvgPath = "/map.svg",
                      Flags.websocketUrl = mconcat ["ws://localhost:5000/g/", roomId, "/ws"]
                    }
                )
            )
        )
          :<|> WebSocket.server roomVar
    )
      :<|> staticContentServer

api :: Proxy Routes
api = Proxy

staticContentServer :: Server StaticContentRoutes
staticContentServer =
  serveDirectoryWebApp "ui/_build"
    :<|> (serveDirectoryWith ((defaultFileServerSettings "ui/static") {ssUseHash = True}))
