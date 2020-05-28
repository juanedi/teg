module Player exposing
    ( Player
    , toRequestParam
    )

import Api
import Json.Encode as Encode


type alias Player =
    Api.Player


toRequestParam : Player -> String
toRequestParam player =
    player
        |> Api.jsonEncPlayer
        |> Encode.encode 0
        |> String.dropLeft 1
        |> String.dropRight 1
