{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Player (Player (..)) where

import Data.Text (Text)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Server.Serialization as Serialization
import Web.HttpApiData (FromHttpApiData (..))

data Player
  = Red
  | Blue
  deriving (Eq)

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Player

instance FromHttpApiData Player where
  parseUrlPiece :: Text -> Either Text Player
  parseUrlPiece fragment =
    if fragment == "red"
      then Right Red
      else
        if fragment == "blue"
          then Right Blue
          else Left ("Could not interpret player from url fragment '" <> fragment <> "''")
