port module Main exposing (main)

import Api
import Board
import Browser
import Country exposing (Country)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Player exposing (Player)
import Time


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitLobbySocket
    | InitGameSocket Player


type PortInfo
    = LobbyStateUpdate (List Player)
    | GameStateUpdate Api.Room


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
    | -- user is deciding which player/color to use
      Lobby (List Player)
    | Joining Player
    | Loaded Api.Room


type Msg
    = JoinResponse (Result Http.Error ())
    | PortInfoReceived Decode.Value
    | PlayerPicked Player
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
    , sendPortCommand (encodePortCommand InitLobbySocket)
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

                    "lobby_state_update" ->
                        Decode.field
                            "data"
                            (Decode.map LobbyStateUpdate (Decode.list Api.jsonDecPlayer))

                    _ ->
                        Decode.fail ("Could interpret port info with tag: " ++ tag)
            )


encodePortCommand : PortCommand -> Value
encodePortCommand cmd =
    case cmd of
        InitLobbySocket ->
            Encode.object
                [ ( "tag", Encode.string "init_lobby_socket" ) ]

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
            case model.gameState of
                Joining player ->
                    case result of
                        Ok _ ->
                            ( { model | gameState = Loading }
                            , InitGameSocket player
                                |> encodePortCommand
                                |> sendPortCommand
                            )

                        Err _ ->
                            -- TODO: handle error
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PortInfoReceived jsonValue ->
            case Decode.decodeValue decodePortInfo jsonValue of
                Ok (LobbyStateUpdate freeSlots) ->
                    case model.gameState of
                        Loading ->
                            ( { model | gameState = Lobby freeSlots }
                            , Cmd.none
                            )

                        Lobby _ ->
                            ( { model | gameState = Lobby freeSlots }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Ok (GameStateUpdate room) ->
                    ( { model | gameState = Loaded room }
                    , Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        PlayerPicked player ->
            ( { model | gameState = Joining player }
            , Api.postJoinByPlayer (Player.toUrlSegment player) JoinResponse
            )

        PaintCountryResponse result ->
            -- TODO: handle error
            ( model, Cmd.none )

        Clicked country ->
            case model.gameState of
                Loading ->
                    ( model, Cmd.none )

                Lobby _ ->
                    ( model, Cmd.none )

                Joining _ ->
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

        Lobby freeSlots ->
            Html.div []
                (List.append
                    [ Html.text "Please pick a color" ]
                    (List.map
                        (\slot ->
                            Html.button
                                [ Events.onClick (PlayerPicked slot) ]
                                [ Html.text (Player.toUrlSegment slot) ]
                        )
                        freeSlots
                    )
                )

        Joining player ->
            Html.text ("Joining as " ++ Player.toUrlSegment player)

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
