{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( Server.run,
    APIRoutes,
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy
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
import Server.State (State)
import qualified Server.State as State
import System.Environment (lookupEnv)

type APIRoutes =
  "join" :> Post '[JSON] Game.LocalState
    -- NOTE: servant-elm forces us to send a string here and parse manually
    :<|> "state" :> Capture "player" Text :> Get '[JSON] Game.LocalState
    :<|> "paint" :> ReqBody '[JSON] (Player, Country) :> Post '[JSON] Game.LocalState

type Routes =
  APIRoutes
    :<|> "_build" :> Raw
    :<|> Raw

run :: IO ()
run = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  serverState <- TVar.newTVarIO State.init
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port
            $ setLogger logger
            $ defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings (app serverState)

app :: TVar State -> Application
app serverState = serve api (server serverState)

api :: Proxy Routes
api = Proxy

server :: TVar State -> Server Routes
server serverState =
  ( join serverState
      :<|> getState serverState
      :<|> paintCountry serverState
  )
    :<|> serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

join :: TVar State -> Handler Game.LocalState
join stateVar =
  withState stateVar $ \state ->
    case Game.join (State.gameState state) of
      Left err ->
        ( Left err,
          state
        )
      Right (localState, updatedGameState) ->
        ( Right localState,
          state {State.gameState = updatedGameState}
        )

getState :: TVar State -> Text -> Handler Game.LocalState
getState stateVar playerId = do
  player <- parsePlayerFromUrl playerId
  withState stateVar $ \state ->
    let gameState = State.gameState state
     in ( Right (Game.playerState player (State.gameState state)),
          state
        )

parsePlayerFromUrl :: Text -> Handler Player
parsePlayerFromUrl playerId =
  case parseUrlPiece playerId of
    Right player -> pure player
    Left err -> throwError (err400 {errBody = encodeErrorMsg err})

paintCountry :: TVar State -> (Player, Country) -> Handler Game.LocalState
paintCountry stateVar (player, country) =
  withState stateVar $ \state ->
    let gameState = State.gameState state
     in case Game.paintCountry player country gameState of
          Left err -> (Left err, state)
          Right updatedGameState ->
            ( Right (Game.playerState player updatedGameState),
              state {State.gameState = updatedGameState}
            )

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

withState ::
  TVar State ->
  (State -> (Either Game.Error a, State)) ->
  Handler a
withState stateVar fn = do
  result <- liftIO (STM.atomically (STM.stateTVar stateVar fn))
  case result of
    Left err -> handleError err
    Right value -> pure value
