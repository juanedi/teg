module Board exposing
    ( init
    , onCountryClicked
    , onCountryMouseEnter
    , onCountryMouseLeave
    , view
    )

import Country exposing (Country)
import Css exposing (fill, hex, property)
import Css.Global exposing (class, id, selector)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Html.Styled exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode


type Board msg
    = Board
        { svgPath : String
        , onCountryClicked : Maybe (Country -> msg)
        , onCountryMouseEnter : Maybe (Country -> msg)
        , onCountryMouseLeave : Maybe (Country -> msg)
        }


init : String -> Board msg
init svgPath =
    Board
        { svgPath = svgPath
        , onCountryClicked = Nothing
        , onCountryMouseEnter = Nothing
        , onCountryMouseLeave = Nothing
        }


onCountryClicked : (Country -> msg) -> Board msg -> Board msg
onCountryClicked handler (Board guts) =
    Board { guts | onCountryClicked = Just handler }


onCountryMouseEnter : (Country -> msg) -> Board msg -> Board msg
onCountryMouseEnter handler (Board guts) =
    Board { guts | onCountryMouseEnter = Just handler }


onCountryMouseLeave : (Country -> msg) -> Board msg -> Board msg
onCountryMouseLeave handler (Board guts) =
    Board { guts | onCountryMouseLeave = Just handler }


view : Board msg -> Html.Styled.Html msg
view ((Board guts) as board) =
    Html.Styled.fromUnstyled
        (Html.node "teg-board"
            (Attr.property "svgPath" (Encode.string guts.svgPath) :: attributes board)
            [ Css.Global.global styles
                |> -- NOTE: the stylesheet generation breaks when usinng a styled
                   -- node here.
                   Html.Styled.toUnstyled
            ]
        )


attributes : Board msg -> List (Html.Attribute msg)
attributes (Board guts) =
    let
        countryEventDecoder =
            Decode.field "detail" Country.decoder
    in
    List.concat
        [ guts.onCountryClicked
            |> Maybe.map (\handler -> [ Events.on "country-clicked" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        , guts.onCountryMouseEnter
            |> Maybe.map (\handler -> [ Events.on "country-mouseenter" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        , guts.onCountryMouseLeave
            |> Maybe.map (\handler -> [ Events.on "country-mouseleave" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        ]


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
