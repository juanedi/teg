module Ui.Button exposing (Size(..), button, submit)

import Css exposing (px)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Ui.Theme as Theme


type Size
    = Large
    | Small


submit :
    { label : String
    , isEnabled : Bool
    , size : Size
    , css : List Css.Style
    }
    -> List (Html.Attribute msg)
    -> Html msg
submit config attrs =
    Html.input
        (List.concat
            [ [ Attributes.type_ "submit"
              , Attributes.value config.label
              , Attributes.disabled (not config.isEnabled)
              , css (styles config)
              ]
            , attrs
            ]
        )
        []


button :
    { label : String
    , onClick : Maybe msg
    , isEnabled : Bool
    , size : Size
    , css : List Css.Style
    }
    -> List (Html.Attribute msg)
    -> Html msg
button config attrs =
    Html.button
        (List.concat
            [ [ Attributes.disabled (not config.isEnabled)
              , css (styles config)
              ]
            , case config.onClick of
                Nothing ->
                    []

                Just msg ->
                    [ Events.onClick msg ]
            , attrs
            ]
        )
        [ Html.text config.label
        ]


styles :
    { a
        | size : Size
        , isEnabled : Bool
        , css : List Css.Style
    }
    -> List Css.Style
styles { isEnabled, size, css } =
    List.concat
        [ [ Css.property "font" "unset"
          , Css.cursor Css.pointer
          , Css.borderStyle Css.none
          , Css.borderRadius (px 6)
          , Css.fontWeight Css.bold
          , Css.color Theme.white
          ]
        , if isEnabled then
            [ Css.backgroundColor (Css.hex "#1293D8")
            , Css.boxShadow5 Css.inset (px -1) (px -1) (px 4) (Css.hex "#0e73a9")
            ]

          else
            [ Css.backgroundColor Theme.grey
            ]
        , case size of
            Large ->
                [ Css.height (px 60)
                , Css.minWidth (px 100)
                , Css.padding2 Css.zero (px 18)
                ]

            Small ->
                [ Css.height (px 36)
                , Css.padding2 Css.zero (px 10)
                , Css.fontSize (px 15)
                ]
        , css
        ]
