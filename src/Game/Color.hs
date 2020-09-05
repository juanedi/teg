{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Color (Color, Game.Color.all) where

import Data.Foldable (find)
import Data.String (fromString)
import Data.Text (Text)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Server.Serialization as Serialization
import Web.HttpApiData (FromHttpApiData (..))

data Color
  = Blue
  | Red
  | Black
  | Yellow
  | Green
  | Magenta
  deriving (Eq, Ord, Show)

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Color

instance FromHttpApiData Color where
  parseUrlPiece :: Text -> Either Text Color
  parseUrlPiece fragment =
    case find (matchesUrlPiece fragment) Game.Color.all of
      Just player ->
        Right player
      Nothing ->
        Left ("Could not interpret player from url fragment '" <> fragment <> "''")

matchesUrlPiece :: Text -> Color -> Bool
matchesUrlPiece fragment player =
  fromString (Serialization.tagToApiLabel (show player)) == fragment

all :: [Color]
all =
  [ Blue,
    Red,
    Black,
    Yellow,
    Green,
    Magenta
  ]
