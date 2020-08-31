{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Player (Player, Game.Player.all) where

import Data.Foldable (find)
import Data.String (fromString)
import Data.Text (Text)
import Elm.Derive (constructorTagModifier, defaultOptions, deriveBoth)
import qualified Server.Serialization as Serialization
import Web.HttpApiData (FromHttpApiData (..))

data Player
  = Blue
  | Red
  | Black
  | Yellow
  | Green
  | Magenta
  deriving (Eq, Ord, Show)

deriveBoth defaultOptions {constructorTagModifier = Serialization.tagToApiLabel} ''Player

instance FromHttpApiData Player where
  parseUrlPiece :: Text -> Either Text Player
  parseUrlPiece fragment =
    case find (matchesUrlPiece fragment) Game.Player.all of
      Just player ->
        Right player
      Nothing ->
        Left ("Could not interpret player from url fragment '" <> fragment <> "''")

matchesUrlPiece :: Text -> Player -> Bool
matchesUrlPiece fragment player =
  fromString (Serialization.tagToApiLabel (show player)) == fragment

all :: [Player]
all =
  [ Blue,
    Red,
    Black,
    Yellow,
    Green,
    Magenta
  ]
