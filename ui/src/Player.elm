module Player exposing
    ( Player
    , colors
    , label
    , toUrlSegment
    )

import Api
import Css
import Json.Encode as Encode
import Ui.Color as Color


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


colors : Player -> { light : Css.Color, solid : Css.Color }
colors player =
    case player of
        Api.Blue ->
            { solid = Color.blueSolid
            , light = Color.blueLight
            }

        Api.Red ->
            { solid = Color.redSolid
            , light = Color.redLight
            }

        Api.Black ->
            { solid = Color.blackSolid
            , light = Color.blackLight
            }

        Api.Yellow ->
            { solid = Color.yellowSolid
            , light = Color.yellowLight
            }

        Api.Green ->
            { solid = Color.greenSolid
            , light = Color.greenLight
            }

        Api.Magenta ->
            { solid = Color.magentaSolid
            , light = Color.magentaLight
            }
