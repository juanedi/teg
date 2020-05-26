{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Codegen (run) where

import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Game
import Servant.Elm (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, generateElmModuleWith)
import Server (APIRoutes)
import System.Process (createProcess, cwd, proc)

run :: IO ()
run =
  let outputDir = "./ui/generated"
   in do
        putStrLn "Deleting old generated code..."
        putStrLn "Generating Elm code from API..."
        generateElmModuleWith
          defElmOptions
          ["Api"]
          defElmImports
          outputDir
          [ DefineElm (Proxy :: Proxy Game.State),
            DefineElm (Proxy :: Proxy Game.Country),
            DefineElm (Proxy :: Proxy Game.Player)
          ]
          (Proxy :: Proxy APIRoutes)
        putStrLn "Formatting generated code using elm-format..."
        createProcess (proc "elm-format" ["Api.elm", "--yes"]) {cwd = Just outputDir}
        putStrLn "Done!"
