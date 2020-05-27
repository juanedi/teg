{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Player (Player (..)) where

import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Server.Serialization as Serialization

data Player
  = Red
  | Blue
  deriving (Eq)

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Player
