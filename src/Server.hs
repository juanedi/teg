{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( Server.run,
    APIRoutes,
  )
where

import qualified Data.ByteString.Lazy
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Game
import Game (Color, Country)
import Game.Room (Room)
import qualified Game.Room as Room
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, IPAddrSource (..), LogType' (..), apacheLogger, initLogger)
import Result (Error (..), Result (..))
import Servant
import qualified Server.FullDuplexWebSocket as FullDuplexWebSocket
import Server.State (State)
import qualified Server.State as State
import qualified Server.WebSocket as WebSocket
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import qualified System.FilePath.Posix as FilePath
import qualified System.Log.FastLogger.Date as Date
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (ssUseHash)

type APIRoutes =
  "join" :> Capture "color" Text :> Capture "name" Text :> Post '[JSON] ()
    :<|> "start" :> Post '[JSON] ()
    :<|> "paint" :> ReqBody '[JSON] (Color, Country) :> PostNoContent '[JSON] ()

type StaticContentRoutes =
  "_build" :> Raw
    :<|> Raw

type Routes = APIRoutes :<|> WebSocket.Routes :<|> FullDuplexWebSocket.WebSocketApi :<|> StaticContentRoutes

{- Represents a pure computation that depends on the current state.

   It can return a modify state and signal an error, but doesn't allow to
   perform IO.
-}
type Action val = Room -> Result Room val

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
    gameApiServer (runAction state)
      :<|> WebSocket.server state
      :<|> FullDuplexWebSocket.server
      :<|> staticContentServer

api :: Proxy Routes
api = Proxy

gameApiServer :: (forall a. Action a -> Handler a) -> Server APIRoutes
gameApiServer runAction =
  (\color name -> runAction (joinGame color name))
    :<|> runAction startGame
    :<|> runAction . paintCountry

staticContentServer :: Server StaticContentRoutes
staticContentServer =
  serveDirectoryWebApp "ui/_build"
    :<|> (serveDirectoryWith ((defaultFileServerSettings "ui/static") {ssUseHash = True}))

joinGame :: Text -> Text -> Action ()
joinGame colorId name room =
  case parseUrlPiece colorId :: Either Text Color of
    Right color ->
      Room.join color name room
    Left err ->
      ( Left (InvalidMove ("Could not parse player from url param")),
        room
      )

startGame :: Action ()
startGame room =
  Room.startGame room

paintCountry :: (Color, Country) -> Action ()
paintCountry (player, country) =
  Room.updateGame
    ( \gameState ->
        case Game.paintCountry player country gameState of
          Left err ->
            ( Left err,
              gameState
            )
          Right gameState' ->
            ( Right (),
              gameState'
            )
    )

handleError :: Error -> Handler a
handleError gameError =
  case gameError of
    InvalidMove msg ->
      throwError (err400 {errBody = encodeErrorMsg msg})
    InternalError msg ->
      throwError (err500 {errBody = encodeErrorMsg msg})

encodeErrorMsg :: Text -> Data.ByteString.Lazy.ByteString
encodeErrorMsg msg =
  encodeUtf8 (Data.Text.Lazy.fromStrict msg)

runAction :: State -> Action a -> Handler a
runAction state action = do
  response <-
    State.runSTM $
      State.updateRoom
        (\room -> pure (action room))
        state
  case response of
    Left err -> handleError err
    Right value -> pure value
