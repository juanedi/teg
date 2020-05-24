{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( runServer
    , runCodeGen
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

data Book = Book
    { name :: String
    }

data Country
  = Argentina
  | Kamchatka

deriveBoth defaultOptions ''Book
deriveBoth defaultOptions ''Country

type API' = "books" :> Get '[JSON] Book
       :<|> "countries" :> Get '[JSON] Country

type API = "_build" :> Raw
      :<|> "books" :> Get '[JSON] Book
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

runCodeGen :: IO ()
runCodeGen = do
  putStrLn "Generating Elm code from API"
  generateElmModuleWith
    defElmOptions
    [ "Generated"
    , "MyApi"
    ]
    defElmImports
    "my-elm-dir"
    [ DefineElm (Proxy :: Proxy Book)
    , DefineElm (Proxy :: Proxy Country)
    ]
    (Proxy :: Proxy API')
  putStrLn "Done!"

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectoryWebApp "frontend/_build"
    :<|> return (Book "The Bible")
    :<|> serveDirectoryFileServer "frontend/static"
