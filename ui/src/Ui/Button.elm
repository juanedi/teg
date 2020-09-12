module Ui.Button exposing (..)

import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events


view :
    { label : String
    , onClick : Maybe msg
    , isEnabled : Bool
    , css : List Css.Style
    }
    -> Html msg
view config =
    Html.button
        (List.concat
            [ [ css
                    (List.concat
                        [ [ Css.property "font" "unset"
                          , Css.height (px 60)
                          , Css.minWidth (px 100)
                          , Css.cursor Css.pointer
                          ]
                        , config.css
                        ]
                    )
              ]
            , [ Attributes.disabled (not config.isEnabled) ]
            , case config.onClick of
                Nothing ->
                    []

                Just msg ->
                    [ Events.onClick msg ]
            ]
        )
        [ Html.text config.label
        ]
