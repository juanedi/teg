module Homepage exposing (..)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Ui.Button as Button
import Ui.Modal as Modal


type alias Model =
    ()


type alias Msg =
    ()


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ css
            [ Css.fontFamilies [ "Antic Slab", "serif" ] ]
        ]
        [ Modal.view
            [ Html.form
                [ Attr.method "post"
                , Attr.action "g"
                ]
                [ Html.h1 [] [ Html.text "T.E.G." ]
                , Button.submit
                    { label = "Crear juego"
                    , isEnabled = True
                    , css = []
                    }
                    [ Attr.autofocus True ]
                ]
            ]
        ]
