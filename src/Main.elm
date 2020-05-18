module Main exposing (main)

import Browser
import Html


type alias Flags =
    ()


type alias Model =
    ()


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( (), Cmd.none )
        , view = \_ -> Html.text "Hi!"
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
