module Player exposing
    ( Player
    , color
    , label
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


label : Player -> String
label player =
    case player of
        Api.Red ->
            "Rojo"

        Api.Blue ->
            "Azul"


color : Player -> { red : Float, green : Float, blue : Float }
color player =
    case player of
        Api.Red ->
            { red = 1, green = 0, blue = 0 }

        Api.Blue ->
            { red = 0, green = 0, blue = 1 }
