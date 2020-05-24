{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game
  ( State,
    Country,
    new,
    tagToApiLabel,
  )
where

import qualified Text.Casing as Casing

data Country
  = Argentina
  | Rusia
  | Kamchatka

data State = State
  { lastClickedCountry :: Maybe Country,
    hoveredCountry :: Maybe Country
  }

new :: State
new =
  State
    { lastClickedCountry = Just Argentina,
      hoveredCountry = Nothing
    }

tagToApiLabel :: String -> String
tagToApiLabel constructorTag =
  Casing.toQuietSnake (Casing.fromHumps constructorTag)
