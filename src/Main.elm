module Main exposing (main)

import Board
import Browser
import Country exposing (Country)
import Html.Styled as Html exposing (Html)


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    }


type Msg
    = CountryClicked Country


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
    case msg of
        CountryClicked country ->
            let
                _ =
                    Debug.log "country clicked" country
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Board.view model.boardSvgPath
        [ Board.onCountryClicked CountryClicked ]
