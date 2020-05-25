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
        , property "stroke" "#beddeb"
        , property "stroke-width" "2px"
        , property "fill-opacity" "20%"
        ]
    , id "continents"
        [ Css.pointerEventsAll
        ]
    , selector "#continents *"
        [ property "fill-opacity" "40%"
        , property "stroke-width" "1px"
        , fill (hex "#FFFFFF")
        ]
    , selector "#continents > #north_america * "
        [ property "stroke" "#ca9782"
        , fill (hex "#ca9782")
        ]
    , selector "#continents > #south_america *"
        [ property "stroke" "#848585"
        , fill (hex "#848585")
        ]
    , selector "#continents > #asia *"
        [ property "stroke" "#868485"
        , fill (hex "#868485")
        ]
    , selector "#continents > #oceania *"
        [ property "stroke" "#7badc7"
        , fill (hex "#7badc7")
        ]
    , selector "#continents > #europe *"
        [ property "stroke" "#b2a7b4"
        , fill (hex "#b2a7b4")
        ]
    , selector "#continents > #africa *"
        [ property "stroke" "#ab8c78"
        , fill (hex "#ab8c78")
        ]
    ]


highlightedCountryStyles : Country -> List Css.Global.Snippet
highlightedCountryStyles country =
    let
        styles =
            [ property "fill-opacity" "80%"
            , property "stroke-width" "1px"
            ]
    in
    [ -- matches paths and polygons
      selector ("#" ++ Country.svgId country ++ ":empty") styles
    , -- matches groups (needed for countries that consist of more than one element)
      selector ("g#" ++ Country.svgId country ++ " *") styles
    ]
