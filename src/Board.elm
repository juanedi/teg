module Board exposing (view)

import Css
import Css.Global exposing (class, id, selector)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Styled
import Json.Encode as Encode


view : String -> Html msg
view svgPath =
    Html.node "teg-board"
        [ Attr.property "svgPath" (Encode.string svgPath) ]
        [ Css.Global.global styles
            |> Html.Styled.toUnstyled
        ]


styles : List Css.Global.Snippet
styles =
    [ id "borde_interno"
        [ Css.fill (Css.hex "#beddeb")
        , Css.property "stroke-dasharray" "3"
        , Css.property "stroke-opacity" "50%"
        , Css.property "stroke-width" "0.5px"
        ]
    , id "paises"
        [ Css.pointerEventsAll
        ]
    , selector "#paises *"
        [ Css.property "fill-opacity" "60%"
        ]
    , selector "#paises > #america_norte * "
        [ Css.fill (Css.hex "#ca9782")
        ]
    , selector "#paises > #america_sur *"
        [ Css.fill (Css.hex "#848585")
        ]
    , selector "#paises > #asia *"
        [ Css.fill (Css.hex "#868485")
        ]
    , selector "#paises > #oceania *"
        [ Css.fill (Css.hex "#7badc7")
        ]
    , selector "#paises > #europa *"
        [ Css.fill (Css.hex "#b2a7b4")
        ]
    , selector "#paises > #africa *"
        [ Css.fill (Css.hex "#ab8c78")
        ]
    , class "active-country"
        [ Css.property "fill-opacity" "100% !important"
        , Css.property "stroke-width" "1px"
        ]
    ]
