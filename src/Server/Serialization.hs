module Server.Serialization (tagToApiLabel) where

import qualified Text.Casing as Casing

tagToApiLabel :: String -> String
tagToApiLabel constructorTag =
  Casing.toQuietSnake (Casing.fromHumps constructorTag)
