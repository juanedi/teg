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
        Api.Blue ->
            "Azul"

        Api.Red ->
            "Rojo"

        Api.Black ->
            "Negro"

        Api.Yellow ->
            "Amarillo"

        Api.Green ->
            "Verde"

        Api.Magenta ->
            "Magenta"


color : Player -> { red : Float, green : Float, blue : Float }
color player =
    case player of
        Api.Blue ->
            { red = 0, green = 0, blue = 1 }

        Api.Red ->
            { red = 1, green = 0, blue = 0 }

        Api.Black ->
            { red = 0, green = 0, blue = 0 }

        Api.Yellow ->
            { red = 1, green = 1, blue = 0 }

        Api.Green ->
            { red = 0, green = 1, blue = 0 }

        Api.Magenta ->
            { red = 1, green = 0, blue = 1 }
