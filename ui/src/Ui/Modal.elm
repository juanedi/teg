module Ui.Modal exposing (view)

import Css exposing (px, zero)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Ui.Theme as Theme


view : List (Html msg) -> Html msg
view contents =
    div
        [ css
            [ Css.position Css.fixed
            , Css.top zero
            , Css.left zero
            , Css.width (Css.vw 100)
            , Css.height (Css.vh 100)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.fontSize (px 18)
            , Css.backgroundColor Theme.backdrop
            , Css.textAlign Css.center
            ]
        ]
        [ div
            [ css
                [ Css.backgroundColor (Css.rgb 255 255 255)
                , Css.padding2 (px 15) (px 20)
                , Css.borderRadius (px 8)
                , Css.boxShadow4 (px 1) (px 1) (px 2) (px 1)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                , Css.minWidth (px 200)
                ]
            ]
            contents
        ]
