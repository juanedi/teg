module Main exposing (main)

import Api
import Board
import Browser
import Country exposing (Country)
import Css
import GameState exposing (GameState)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Http
import Time


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , hoveredCountry : Maybe Country
    , gameState : GameState
    }


type Msg
    = ServerStateResponse (Result Http.Error Api.LocalState)
    | PollServerState
    | Clicked Country
    | MouseEntered Country
    | MouseLeft Country


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = \_ -> Time.every 1000 (\_ -> PollServerState)
        }


init : Flags -> ( Model, Cmd Msg )
init { boardSvgPath } =
    ( { boardSvgPath = boardSvgPath
      , hoveredCountry = Nothing
      , gameState = GameState.Loading
      }
    , Api.postJoin ServerStateResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerStateResponse result ->
            case result of
                Ok localState ->
                    ( { model | gameState = GameState.Loaded localState }
                    , Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        PollServerState ->
            case model.gameState of
                GameState.Loading ->
                    ( model, Cmd.none )

                GameState.Loaded { identity } ->
                    ( model
                    , Api.getState identity ServerStateResponse
                    )

        Clicked country ->
            case model.gameState of
                GameState.Loading ->
                    ( model, Cmd.none )

                GameState.Loaded { identity } ->
                    ( model
                    , Api.postPaint ( identity, country ) ServerStateResponse
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
            [ Css.height (Css.vh 100)
            , Css.width (Css.vw 100)
            , Css.position Css.fixed
            , Css.top Css.zero
            , Css.left Css.zero
            , Css.backgroundColor (Css.hex "#e9f0f0")
            ]
        ]
        [ Board.view
            { svgPath = model.boardSvgPath
            , onCountryClicked = Just Clicked
            , onCountryMouseEnter = Just MouseEntered
            , onCountryMouseLeave = Just MouseLeft
            , highlightedCoutries =
                List.concat
                    [ model.hoveredCountry
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    , case model.gameState of
                        GameState.Loading ->
                            []

                        GameState.Loaded { paintedCountries } ->
                            paintedCountries
                                |> List.map Tuple.first
                    ]
            , styles =
                [ Css.height (Css.pct 100)
                , Css.width (Css.pct 100)
                ]
            }
            |> Html.fromUnstyled
        ]
