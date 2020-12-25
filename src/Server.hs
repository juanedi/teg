{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Server (Server.run) where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.UUID
import qualified Data.UUID.V4
import Game.Room (Room)
import qualified Game.Room as Room
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, IPAddrSource (..), LogType (..), apacheLogger, initLogger)
import Network.WebSockets.Connection (PendingConnection, acceptRequest, rejectRequest, withPingThread)
import Servant
import Servant.HTML.Blaze
import qualified Server.Flags as Flags
import Server.PostRedirect
import qualified Server.Templates as Templates
import qualified Server.WebSocket as WebSocket
import qualified System.Directory as Directory
import System.Environment (getEnv, lookupEnv)
import qualified System.FilePath.Posix as FilePath
import qualified System.Log.FastLogger.Date as Date
import qualified Text.Blaze.Html5 as H
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssUseHash)

{- ORMOLU_DISABLE -}
type Routes = Get '[HTML] H.Html
         :<|> ( "g" :> ( PostRedirect 301 String
                  :<|> ( Capture "roomId" Room.Id :> ( Get '[HTML] H.Html
                                                :<|> "ws" :> WebSocket.WebSocketApi))))
         :<|> Raw

{- ORMOLU_ENABLE -}

data State = State
  { rooms :: TVar (Map Room.Id (TVar Room)),
    httpUrlBase :: Text,
    wsUrlBase :: Text,
    assetsRoot :: FilePath,
    port :: Int
  }

run :: IO ()
run = do
  httpUrlBase <- getEnv "HTTP_URL_BASE"
  wsUrlBase <- getEnv "WS_URL_BASE"
  assetsRoot <- getEnv "ASSETS_ROOT"
  maybePort <- lookupEnv "PORT"
  let port = maybe 8080 read maybePort
  logFile <- lookupEnv "REQUESTS_LOG"
  logger <- initializeLogger (fromMaybe "./requests.log" logFile)
  rooms <- STM.atomically (newTVar Map.empty)
  let settings =
        setPort port $
          setLogger logger $
            defaultSettings
  let state =
        State
          { rooms = rooms,
            httpUrlBase = pack httpUrlBase,
            wsUrlBase = pack wsUrlBase,
            assetsRoot = assetsRoot,
            port = port
          }
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

{- ORMOLU_DISABLE -}
app :: State -> Application
app state =
  serve api $
        home
  :<|> ( createRoom state
    :<|> ( \roomId -> showRoom state roomId
                 :<|> websocket state roomId
                )
        )
  :<|> staticContentServer (assetsRoot state)
{- ORMOLU_ENABLE -}

home :: Handler H.Html
home =
  return Templates.home

createRoom :: State -> Handler (RedirectResponse String)
createRoom state = do
  uuid <- liftIO (fmap Data.UUID.toText Data.UUID.V4.nextRandom)
  let roomId = Room.Id uuid
  liftIO $
    STM.atomically $ do
      newRoom <- Room.init
      newRoomVar <- newTVar newRoom
      modifyTVar (rooms state) (Map.insert roomId newRoomVar)
  redirect ("/g/" ++ unpack uuid)

showRoom :: State -> Room.Id -> Handler H.Html
showRoom state roomId = do
  maybeRoom <- fetchRoom state roomId
  let (Room.Id uuid) = roomId
  case maybeRoom of
    Just _ ->
      return
        ( Templates.game
            ( Flags.Flags
                { Flags.boardSvgPath = "/map.svg",
                  Flags.roomUrl = mconcat [httpUrlBase state, "/g/", uuid, "/"],
                  Flags.websocketUrl = mconcat [wsUrlBase state, "/g/", uuid, "/ws"]
                }
            )
        )
    Nothing ->
      throwError $
        ServerError
          { errHTTPCode = 404,
            errReasonPhrase = "That room doesn't exist.",
            errBody = "That room doesn't exist. Maybe the game already finished or you got an invalid link?",
            errHeaders = []
          }

websocket :: State -> Room.Id -> PendingConnection -> Handler ()
websocket state roomId pc = liftIO $ do
  maybeRoom <- fetchRoom state roomId
  case maybeRoom of
    Nothing ->
      rejectRequest pc "The room does not exist"
    (Just roomVar) ->
      do
        connection <- acceptRequest pc
        withPingThread connection 10 (return ()) $ do
          WebSocket.initChannel roomVar connection

fetchRoom :: MonadIO m => State -> Room.Id -> m (Maybe (TVar Room))
fetchRoom state roomId = do
  liftIO $
    STM.atomically $ do
      rooms <- readTVar (rooms state)
      return (Map.lookup roomId rooms)

api :: Proxy Routes
api = Proxy

staticContentServer :: FilePath -> Server Raw
staticContentServer root =
  serveDirectoryWith ((defaultFileServerSettings root) {ssUseHash = True})
