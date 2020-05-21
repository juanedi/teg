module Board exposing (view)

import Css exposing (fill, hex, property)
import Css.Global exposing (class, id, selector)
import Html
import Html.Attributes as Attr
import Html.Styled exposing (Html)
import Json.Encode as Encode


view : String -> Html.Styled.Html msg
view svgPath =
    Html.Styled.fromUnstyled
        (Html.node "teg-board"
            [ Attr.property "svgPath" (Encode.string svgPath) ]
            [ Css.Global.global styles
                |> -- NOTE: the stylesheet generation breaks when usinng a styled
                   -- node here.
                   Html.Styled.toUnstyled
            ]
        )


styles : List Css.Global.Snippet
styles =
    [ id "borde_interno"
        [ fill (hex "#beddeb")
        , property "stroke-dasharray" "3"
        , property "stroke-opacity" "50%"
        , property "stroke-width" "0.5px"
        ]
    , id "paises"
        [ Css.pointerEventsAll
        ]
    , selector "#paises *"
        [ property "fill-opacity" "60%"
        ]
    , selector "#paises > #america_norte * "
        [ fill (hex "#ca9782")
        ]
    , selector "#paises > #america_sur *"
        [ fill (hex "#848585")
        ]
    , selector "#paises > #asia *"
        [ fill (hex "#868485")
        ]
    , selector "#paises > #oceania *"
        [ fill (hex "#7badc7")
        ]
    , selector "#paises > #europa *"
        [ fill (hex "#b2a7b4")
        ]
    , selector "#paises > #africa *"
        [ fill (hex "#ab8c78")
        ]
    , class "active-country"
        [ property "fill-opacity" "100% !important"
        , property "stroke-width" "1px"
        ]
    ]
