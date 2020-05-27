module GameState exposing (GameState(..))

import Api


type GameState
    = Loading
    | Loaded Api.LocalState
