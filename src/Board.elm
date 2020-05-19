module Board exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode


view : String -> Html msg
view svgPath =
    Html.node "teg-board"
        [ Attr.property "svgPath" (Encode.string svgPath) ]
        []
