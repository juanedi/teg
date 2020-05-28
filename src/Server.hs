{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
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
import System.Environment (lookupEnv)

type APIRoutes =
  "join" :> Post '[JSON] Game.LocalState
    -- NOTE: servant-elm forces us to send a string here and parse manually
    :<|> "state" :> Capture "player" Text :> Get '[JSON] Game.LocalState
    :<|> "paint" :> ReqBody '[JSON] (Player, Country) :> Post '[JSON] Game.LocalState

type StaticContentRoutes =
  "_build" :> Raw
    :<|> Raw

type Routes = APIRoutes :<|> StaticContentRoutes

{- Represents a pure computation that depends on the current state.

   It can return a modify state and signal an error, but doesn't allow to
   perform IO.
-}
type Action a = Game.State -> Result a

{- The result of a computation over a game.

   It can signal an error or emit a value, and allows to modify the resulting
   state regardless of whether we got an error or not.
-}
type Result a = (Either Game.Error a, Game.State)

run :: IO ()
run = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  serverState <- TVar.newTVarIO Game.init
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port
            $ setLogger logger
            $ defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings (app serverState)

app :: TVar Game.State -> Application
app serverState =
  serve api $
    gameApiServer (runAction serverState) :<|> staticContentServer

api :: Proxy Routes
api = Proxy

gameApiServer :: (forall a. Action a -> Handler a) -> Server APIRoutes
gameApiServer runAction =
  runAction join
    :<|> ( \playerId -> do
             player <- parsePlayerFromUrl playerId
             runAction (getState player)
         )
    :<|> runAction . paintCountry

staticContentServer :: Server StaticContentRoutes
staticContentServer =
  serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"

join :: Action Game.LocalState
join state =
  case Game.join state of
    Left err ->
      ( Left err,
        state
      )
    Right (localState, updatedState) ->
      ( Right localState,
        updatedState
      )

getState :: Player -> Action Game.LocalState
getState player state =
  ( Right (Game.playerState player state),
    state
  )

parsePlayerFromUrl :: Text -> Handler Player
parsePlayerFromUrl playerId =
  case parseUrlPiece playerId of
    Right player -> pure player
    Left err -> throwError (err400 {errBody = encodeErrorMsg err})

paintCountry :: (Player, Country) -> Action Game.LocalState
paintCountry (player, country) state =
  case Game.paintCountry player country state of
    Left err -> (Left err, state)
    Right updatedState ->
      ( Right (Game.playerState player updatedState),
        updatedState
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

runAction :: TVar Game.State -> Action a -> Handler a
runAction stateVar fn = do
  result <- liftIO (STM.atomically (STM.stateTVar stateVar fn))
  case result of
    Left err -> handleError err
    Right value -> pure value
