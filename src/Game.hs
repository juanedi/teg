{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Game (Country) where

import Elm.Derive (defaultOptions, deriveBoth)

data Country
  = Argentina
  | Rusia
  | Kamchatka

deriveBoth defaultOptions ''Country
