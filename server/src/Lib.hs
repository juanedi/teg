{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant

type API = "_build" :> Raw
      :<|> Raw

startApp :: Int -> IO ()
startApp port = do
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port $
          setLogger logger $
          defaultSettings
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectoryWebApp "frontend/_build"
    :<|> serveDirectoryFileServer "frontend/static"
