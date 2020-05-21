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
    [ id "inner_border"
        [ fill (hex "#beddeb")
        , property "stroke-dasharray" "3"
        , property "stroke-opacity" "50%"
        , property "stroke-width" "0.5px"
        ]
    , id "continents"
        [ Css.pointerEventsAll
        ]
    , selector "#continents *"
        [ property "fill-opacity" "60%"
        ]
    , selector "#continents > #north_america * "
        [ fill (hex "#ca9782")
        ]
    , selector "#continents > #south_america *"
        [ fill (hex "#848585")
        ]
    , selector "#continents > #asia *"
        [ fill (hex "#868485")
        ]
    , selector "#continents > #australia *"
        [ fill (hex "#7badc7")
        ]
    , selector "#continents > #europe *"
        [ fill (hex "#b2a7b4")
        ]
    , selector "#continents > #africa *"
        [ fill (hex "#ab8c78")
        ]
    , class "active-country"
        [ property "fill-opacity" "100% !important"
        , property "stroke-width" "1px"
        ]
    ]
