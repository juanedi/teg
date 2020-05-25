module GameState exposing
    ( GameState(..)
    , ServerState
    )

import Api


type GameState
    = Loading
    | Loaded Api.State


type alias ServerState =
    Api.State
