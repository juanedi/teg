module GameState exposing
    ( GameState(..)
    , ServerState
    )

import Api


type GameState
    = Loading
    | Loaded
        -- NOTE: this allows us to have values of the form (Red, WaitingForRed) we
        -- should map the server state to a new ClientState type that has exactly
        -- what the client needs (who they are and what is expected of them at
        -- this turn).
        ( Api.Player, Api.State )


type alias ServerState =
    Api.State
