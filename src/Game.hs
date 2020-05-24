{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Game (
    State
  , Country
  , new
) where

import Elm.Derive (defaultOptions, deriveBoth)

data Country
  = Argentina
  | Rusia
  | Kamchatka

deriveBoth defaultOptions ''Country

data State =
  State
  { lastClickedCountry :: Maybe Country
  , hoveredCountry :: Maybe Country
  }

deriveBoth defaultOptions ''State

new :: State
new =
  State
    { lastClickedCountry = Just Argentina
    , hoveredCountry = Nothing
    }
