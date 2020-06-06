port module Main exposing (main)

import Api
import Board
import Browser
import Country exposing (Country)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Player exposing (Player)
import Time


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitGameSocket Player


type PortInfo
    = GameStateUpdate Api.Room


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , hoveredCountry : Maybe Country
    , gameState : GameState
    }


type GameState
    = Loading
    | Loaded Api.Room


type Msg
    = JoinResponse (Result Http.Error Player)
    | PortInfoReceived Decode.Value
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
      , gameState = Loading
      }
    , Api.postJoin JoinResponse
    )


decodePortInfo : Decode.Decoder PortInfo
decodePortInfo =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "game_state_update" ->
                        Decode.field
                            "data"
                            (Decode.map GameStateUpdate Api.jsonDecRoom)

                    _ ->
                        Decode.fail ("Could interpret port info with tag: " ++ tag)
            )


encodePortCommand : PortCommand -> Value
encodePortCommand cmd =
    case cmd of
        InitGameSocket player ->
            Encode.object
                [ ( "tag", Encode.string "init_game_socket" )
                , ( "data"
                  , player
                        |> Player.toUrlSegment
                        |> Encode.string
                  )
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JoinResponse result ->
            case result of
                Ok player ->
                    ( model
                    , case model.gameState of
                        Loading ->
                            InitGameSocket player
                                |> encodePortCommand
                                |> sendPortCommand

                        _ ->
                            Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        PortInfoReceived jsonValue ->
            case Decode.decodeValue decodePortInfo jsonValue of
                Ok (GameStateUpdate room) ->
                    ( { model | gameState = Loaded room }
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
                Loading ->
                    ( model, Cmd.none )

                Loaded Api.WaitingForPlayers ->
                    ( model, Cmd.none )

                Loaded (Api.Started { identity }) ->
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
    portInfo PortInfoReceived


view : Model -> Html Msg
view model =
    case model.gameState of
        Loading ->
            Html.text "Joining the game"

        Loaded Api.WaitingForPlayers ->
            Html.text "Waiting for other players to join"

        Loaded (Api.Started game) ->
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
