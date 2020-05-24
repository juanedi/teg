{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( runServer
    , runCodegen
    , app
    ) where

import Elm.Derive (defaultOptions, deriveBoth)
import Data.Maybe (fromMaybe)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Elm (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, generateElmModuleWith)
import System.Environment (lookupEnv)

data Country
  = Argentina
  | Rusia
  | Kamchatka

deriveBoth defaultOptions ''Country

type API' = "countries" :> Get '[JSON] Country

type API = "_build" :> Raw
      :<|> Raw

runServer :: IO ()
runServer = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (fmap read maybePort)
  withStdoutLogger $ \logger -> do
    let settings =
          setPort port $
          setLogger logger $
          defaultSettings
    putStrLn ("Starting the application at port " ++ show port)
    runSettings settings app

runCodegen :: IO ()
runCodegen = do
  putStrLn "Generating Elm code from API"
  generateElmModuleWith
    defElmOptions
    [ "Teg" , "Api"]
    defElmImports
    "ui/generated"
    [ DefineElm (Proxy :: Proxy Country)
    ]
    (Proxy :: Proxy API')
  putStrLn "Done!"

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectoryWebApp "ui/_build"
    :<|> serveDirectoryFileServer "ui/static"
