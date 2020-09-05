module Color exposing
    ( Color
    , label
    , theme
    , toUrlSegment
    )

import Api
import Css
import Json.Encode as Encode
import Ui.Theme as Theme


type alias Color =
    Api.Color


toUrlSegment : Color -> String
toUrlSegment player =
    player
        |> Api.jsonEncColor
        |> Encode.encode 0
        |> String.dropLeft 1
        |> String.dropRight 1


label : Color -> String
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


theme : Color -> { light : Css.Color, solid : Css.Color }
theme player =
    case player of
        Api.Blue ->
            { solid = Theme.blueSolid
            , light = Theme.blueLight
            }

        Api.Red ->
            { solid = Theme.redSolid
            , light = Theme.redLight
            }

        Api.Black ->
            { solid = Theme.blackSolid
            , light = Theme.blackLight
            }

        Api.Yellow ->
            { solid = Theme.yellowSolid
            , light = Theme.yellowLight
            }

        Api.Green ->
            { solid = Theme.greenSolid
            , light = Theme.greenLight
            }

        Api.Magenta ->
            { solid = Theme.magentaSolid
            , light = Theme.magentaLight
            }
