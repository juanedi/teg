module Main exposing (main)

import Board
import Browser
import Html.Styled as Html exposing (Html)


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    }


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init { boardSvgPath } =
    ( { boardSvgPath = boardSvgPath }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Board.view model.boardSvgPath
