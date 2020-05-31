{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( Server.run,
    APIRoutes,
  )
where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy
import Data.Conduit (ConduitT)
import Data.Conduit (yield)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Game
import Game (Country, Player)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.API.WebSocketConduit (WebSocketConduit)
import Server.State (State)
import qualified Server.State as State
import System.Environment (lookupEnv)

type APIRoutes =
  "join" :> Post '[JSON] Player
    :<|> "paint" :> ReqBody '[JSON] (Player, Country) :> PostNoContent '[JSON] ()

type WebSocketRoutes =
  "socket" :> Capture "player" Player :> WebSocketConduit () Game.LocalState

type StaticContentRoutes =
  "_build" :> Raw
    :<|> Raw

type Routes = APIRoutes :<|> WebSocketRoutes :<|> StaticContentRoutes

{- Represents a pure computation that depends on the current state.

   It can return a modify state and signal an error, but doesn't allow to
   perform IO.
-}
type Action a = Game.State -> Result a

{- The result of a computation over a game.

   It can signal an error or emit a value, and allows to modify the resulting
   state regardless of whether we got an error or not.
-}
data Result a = Result
  { response :: Either Game.Error a,
    newState :: Game.State
  }

run :: IO ()
run = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  gameState <- newTVarIO Game.init
  broadcastChannel <- newBroadcastTChanIO
  let state =
        State.State
          { State.gameState = gameState,
            State.broadcastChannel = broadcastChannel
          }
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port
            $ setLogger logger
            $ defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings (app state)
  where

app :: State -> Application
app state =
  serve api $
    gameApiServer (runAction state)
      :<|> webSocketServer state
      :<|> staticContentServer

api :: Proxy Routes
api = Proxy

gameApiServer :: (forall a. Action a -> Handler a) -> Server APIRoutes
gameApiServer runAction =
  runAction join
    :<|> runAction . paintCountry

webSocketServer :: State -> Server WebSocketRoutes
webSocketServer state = webSocketHandler
  where
    webSocketHandler :: MonadIO m => Player -> ConduitT () Game.LocalState m ()
    webSocketHandler player = do
      (currentGameState, playerChannel) <-
        runSTM
          ( do
              chan <- dupTChan (State.broadcastChannel state)
              gameState <- readTVar (State.gameState state)
              pure (gameState, chan)
          )
      yield (Game.playerState player currentGameState)
      notificationLoop playerChannel player

notificationLoop :: MonadIO m => TChan Game.State -> Player -> ConduitT () Game.LocalState m ()
notificationLoop playerChannel player = do
  gameState <- runSTM (readTChan playerChannel)
  yield $ Game.playerState player gameState
  notificationLoop playerChannel player

staticContentServer :: Server StaticContentRoutes
staticContentServer =
  serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

join :: Action Player
join state =
  case Game.join state of
    Left err ->
      Result
        { response = Left err,
          newState = state
        }
    Right (player, state') ->
      Result
        { response = Right player,
          newState = state'
        }

paintCountry :: (Player, Country) -> Action ()
paintCountry (player, country) state =
  case Game.paintCountry player country state of
    Left err ->
      Result
        { response = Left err,
          newState = state
        }
    Right state' ->
      Result
        { response = Right (),
          newState = state'
        }

handleError :: Game.Error -> Handler a
handleError gameError =
  case gameError of
    Game.InvalidMove msg ->
      throwError (err400 {errBody = encodeErrorMsg msg})
    Game.InternalError msg ->
      throwError (err500 {errBody = encodeErrorMsg msg})

encodeErrorMsg :: Text -> Data.ByteString.Lazy.ByteString
encodeErrorMsg msg =
  encodeUtf8 (Data.Text.Lazy.fromStrict msg)

runAction :: State -> Action a -> Handler a
runAction state action =
  let gameStateVar = State.gameState state
      broadcastChannel = State.broadcastChannel state
   in do
        result <-
          runSTM $
            do
              gameState <- readTVar gameStateVar
              let result = action gameState
              writeTVar gameStateVar (newState result)
              writeTChan broadcastChannel (newState result)
              pure result
        case response result of
          Left err -> handleError err
          Right value -> pure value

runSTM :: MonadIO m => STM a -> m a
runSTM = liftIO . STM.atomically
