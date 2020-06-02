port module Main exposing (main)

import Api
import Board
import Browser
import Country exposing (Country)
import Css
import GameState exposing (GameState)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode as Decode
import Player exposing (Player)
import Time


port initSocket : String -> Cmd msg


port stateUpdates : (Decode.Value -> msg) -> Sub msg


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , hoveredCountry : Maybe Country
    , gameState : GameState
    }


type Msg
    = JoinResponse (Result Http.Error Player)
    | StateUpdate (Result String Api.Room)
    | PaintCountryResponse (Result Http.Error ())
    | Clicked Country
    | MouseEntered Country
    | MouseLeft Country


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { boardSvgPath } =
    ( { boardSvgPath = boardSvgPath
      , hoveredCountry = Nothing
      , gameState = GameState.Loading
      }
    , Api.postJoin JoinResponse
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JoinResponse result ->
            case result of
                Ok player ->
                    ( model
                    , case model.gameState of
                        GameState.Loading ->
                            initSocket (Player.toUrlSegment player)

                        _ ->
                            Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        StateUpdate result ->
            case result of
                Ok room ->
                    ( { model | gameState = GameState.Loaded room }
                    , Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        PaintCountryResponse result ->
            -- TODO: handle error
            ( model, Cmd.none )

        Clicked country ->
            case model.gameState of
                GameState.Loading ->
                    ( model, Cmd.none )

                GameState.Loaded Api.WaitingForPlayers ->
                    ( model, Cmd.none )

                GameState.Loaded (Api.Started { identity }) ->
                    ( model
                    , Api.postPaint ( identity, country ) PaintCountryResponse
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    stateUpdates
        (\value ->
            value
                |> Decode.decodeValue Api.jsonDecRoom
                |> Result.mapError (\_ -> "Could not decode state sent by the websocket")
                |> StateUpdate
        )


view : Model -> Html Msg
view model =
    case model.gameState of
        GameState.Loading ->
            Html.text "Joining the game"

        GameState.Loaded Api.WaitingForPlayers ->
            Html.text "Waiting for other players to join"

        GameState.Loaded (Api.Started game) ->
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
                            , game.paintedCountries
                                |> List.map Tuple.first
                            ]
                    , styles =
                        [ Css.height (Css.pct 100)
                        , Css.width (Css.pct 100)
                        ]
                    }
                    |> Html.fromUnstyled
                ]
