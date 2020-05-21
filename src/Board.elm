module Board exposing
    ( onCountryClicked
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


type Attribute msg
    = OnCountryClicked (Country -> msg)
    | OnCountryMouseEnter (Country -> msg)
    | OnCountryMouseLeave (Country -> msg)


onCountryClicked : (Country -> msg) -> Attribute msg
onCountryClicked =
    OnCountryClicked


onCountryMouseEnter : (Country -> msg) -> Attribute msg
onCountryMouseEnter =
    OnCountryMouseEnter


onCountryMouseLeave : (Country -> msg) -> Attribute msg
onCountryMouseLeave =
    OnCountryMouseLeave


view : String -> List (Attribute msg) -> Html.Styled.Html msg
view svgPath attributes =
    Html.Styled.fromUnstyled
        (Html.node "teg-board"
            (List.append
                [ Attr.property "svgPath" (Encode.string svgPath) ]
                (List.map
                    (\attr ->
                        case attr of
                            OnCountryClicked handler ->
                                Events.on "country-clicked"
                                    (Decode.map handler (Decode.field "detail" Country.decoder))

                            OnCountryMouseEnter handler ->
                                Events.on "country-mouseenter"
                                    (Decode.map handler (Decode.field "detail" Country.decoder))

                            OnCountryMouseLeave handler ->
                                Events.on "country-mouseleave"
                                    (Decode.map handler (Decode.field "detail" Country.decoder))
                    )
                    attributes
                )
            )
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
