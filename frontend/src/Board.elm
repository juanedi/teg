module Board exposing (view)

import Country exposing (Country)
import Css exposing (fill, hex, property)
import Css.Global exposing (class, id, selector)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Styled
import Json.Decode as Decode
import Json.Encode as Encode


view :
    { svgPath : String
    , onCountryClicked : Maybe (Country -> msg)
    , onCountryMouseEnter : Maybe (Country -> msg)
    , onCountryMouseLeave : Maybe (Country -> msg)
    , highlightedCoutries : List Country
    , styles : List Css.Style
    }
    -> Html msg
view config =
    Html.node "teg-board"
        (Attr.property "svgPath" (Encode.string config.svgPath) :: attributes config)
        [ [ staticStyles
          , List.concatMap highlightedCountryStyles config.highlightedCoutries
          ]
            |> List.concat
            |> Css.Global.global
            |> Html.Styled.toUnstyled
        ]


attributes :
    { a
        | onCountryClicked : Maybe (Country -> msg)
        , onCountryMouseEnter : Maybe (Country -> msg)
        , onCountryMouseLeave : Maybe (Country -> msg)
    }
    -> List (Html.Attribute msg)
attributes { onCountryClicked, onCountryMouseEnter, onCountryMouseLeave } =
    let
        countryEventDecoder =
            Decode.field "detail" Country.decoder
    in
    List.concat
        [ onCountryClicked
            |> Maybe.map (\handler -> [ Events.on "country-clicked" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        , onCountryMouseEnter
            |> Maybe.map (\handler -> [ Events.on "country-mouseenter" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        , onCountryMouseLeave
            |> Maybe.map (\handler -> [ Events.on "country-mouseleave" (Decode.map handler countryEventDecoder) ])
            |> Maybe.withDefault []
        ]


staticStyles : List Css.Global.Snippet
staticStyles =
    [ id "inner_border"
        [ fill (hex "#beddeb")
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
    ]


highlightedCountryStyles : Country -> List Css.Global.Snippet
highlightedCountryStyles country =
    let
        styles =
            [ property "fill-opacity" "100% !important"
            , property "stroke-width" "1px"
            ]
    in
    [ -- matches paths and polygons
      selector ("#" ++ Country.svgId country ++ ":empty") styles
    , -- matches groups (needed for countries that consist of more than one element)
      selector ("g#" ++ Country.svgId country ++ " *") styles
    ]
