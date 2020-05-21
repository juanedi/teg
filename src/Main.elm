module Main exposing (main)

import Board
import Browser
import Country exposing (Country)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , lastClickedCountry : Maybe Country
    , hoveredCountry : Maybe Country
    }


type Msg
    = Clicked Country
    | MouseEntered Country
    | MouseLeft Country


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
    ( { boardSvgPath = boardSvgPath
      , lastClickedCountry = Nothing
      , hoveredCountry = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked country ->
            ( { model | lastClickedCountry = Just country }
            , Cmd.none
            )

        MouseEntered country ->
            ( { model | hoveredCountry = Just country }
            , Cmd.none
            )

        MouseLeft country ->
            ( { model
                | hoveredCountry =
                    case model.hoveredCountry of
                        Nothing ->
                            Nothing

                        Just hoveredCountry ->
                            if hoveredCountry == country then
                                Nothing

                            else
                                Just hoveredCountry
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        [ Board.init model.boardSvgPath
            |> Board.withStyles
                [ Css.maxWidth (Css.vw 60)
                ]
            |> Board.onCountryClicked Clicked
            |> Board.onCountryMouseEnter MouseEntered
            |> Board.onCountryMouseLeave MouseLeft
            |> Board.withHighlightedCountries
                (List.filterMap identity
                    [ model.lastClickedCountry
                    , model.hoveredCountry
                    ]
                )
            |> Board.view
        , Html.pre []
            [ Html.text (Debug.toString model) ]
        ]
