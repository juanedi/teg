module Player exposing
    ( Player
    , toUrlSegment
    )

import Api
import Json.Encode as Encode


type alias Player =
    Api.Player


toUrlSegment : Player -> String
toUrlSegment player =
    player
        |> Api.jsonEncPlayer
        |> Encode.encode 0
        |> String.dropLeft 1
        |> String.dropRight 1
