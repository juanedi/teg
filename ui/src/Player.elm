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


color : Player -> { red : Int, green : Int, blue : Int }
color player =
    case player of
        Api.Blue ->
            { red = 0, green = 0, blue = 255 }

        Api.Red ->
            { red = 255, green = 0, blue = 0 }

        Api.Black ->
            { red = 0, green = 0, blue = 0 }

        Api.Yellow ->
            { red = 255, green = 234, blue = 0 }

        Api.Green ->
            { red = 0, green = 255, blue = 0 }

        Api.Magenta ->
            { red = 255, green = 0, blue = 255 }
