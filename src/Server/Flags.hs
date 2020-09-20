{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server.Flags (Flags (..)) where

import Data.Text (Text)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import Server.Serialization (tagToApiLabel)

data Flags = Flags
  { boardSvgPath :: Text,
    roomUrl :: Text,
    websocketUrl :: Text
  }

deriveBoth defaultOptions {constructorTagModifier = tagToApiLabel} ''Flags
