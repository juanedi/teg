module Board exposing (view)

import Country exposing (Country)
import Css exposing (fill, hex, property)
import Css.Global exposing (class, id, selector)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
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
    -> Html.Styled.Html msg
view config =
    Html.Styled.div [ css config.styles ]
        [ Html.Styled.fromUnstyled
            (Html.node "teg-board"
                (Attr.property "svgPath" (Encode.string config.svgPath) :: attributes config)
                [ Css.Global.global (styles config)
                    |> -- NOTE: the stylesheet generation breaks when usinng a styled
                       -- node here.
                       Html.Styled.toUnstyled
                ]
            )
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


styles : { a | highlightedCoutries : List Country } -> List Css.Global.Snippet
styles { highlightedCoutries } =
    List.concat
        [ -- styles for highlighted countries
          List.map
            (\country ->
                id (Country.svgId country)
                    [ property "fill-opacity" "100% !important"
                    , property "stroke-width" "1px"
                    ]
            )
            highlightedCoutries
        , -- static styles
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
          ]
        ]
