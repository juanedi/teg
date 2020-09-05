port module Main exposing (main)

import Api
import Board
import Browser
import Browser.Events
import Country exposing (Country)
import Css exposing (px, zero)
import Gameplay
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Player exposing (Player)
import Ui.Button as Button


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitLobbySocket
    | InitGameSocket Player


type PortInfo
    = LobbyStateUpdate Api.ConnectionStates
    | GameStateUpdate Api.Room


type alias Flags =
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , state : State
    }


type State
    = Loading
    | Lobby LobbyState
    | Joining Player
    | WaitingForPlayers Api.ConnectionStates
    | ReadyToStart Api.ConnectionStates
    | Starting Api.ConnectionStates
    | Playing Gameplay.State


type alias LobbyState =
    { selectedPlayer : Maybe Player
    , connectionStates : Api.ConnectionStates
    }


type Msg
    = JoinResponse (Result Http.Error ())
    | PortInfoReceived Decode.Value
    | PlayerPicked Player
    | JoinGameClicked
    | StartGameClicked
    | StartGameResponse (Result Http.Error ())
    | GameplayMsg Gameplay.Msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { boardSvgPath } =
    ( { boardSvgPath = boardSvgPath
      , state = Loading
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
                            (Decode.map LobbyStateUpdate Api.jsonDecConnectionStates)

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
            case model.state of
                Joining player ->
                    case result of
                        Ok _ ->
                            ( { model | state = Loading }
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
                Ok (LobbyStateUpdate connectionStates) ->
                    case model.state of
                        Loading ->
                            ( { model
                                | state =
                                    Lobby
                                        { selectedPlayer = Nothing
                                        , connectionStates = connectionStates
                                        }
                              }
                            , Cmd.none
                            )

                        Lobby lobbyState ->
                            ( { model | state = Lobby { lobbyState | connectionStates = connectionStates } }
                            , Cmd.none
                            )

                        WaitingForPlayers _ ->
                            ( { model | state = WaitingForPlayers connectionStates }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Ok (GameStateUpdate room) ->
                    case room of
                        Api.WaitingForPlayers connectionStates ->
                            ( { model | state = WaitingForPlayers connectionStates }
                            , Cmd.none
                            )

                        Api.ReadyToStart connectionStates ->
                            ( { model | state = ReadyToStart connectionStates }
                            , Cmd.none
                            )

                        Api.Started game ->
                            ( { model
                                | state =
                                    case model.state of
                                        Playing gameplayState ->
                                            Playing (Gameplay.serverUpdate game gameplayState)

                                        _ ->
                                            Playing (Gameplay.init game)
                              }
                            , Cmd.none
                            )

                Err _ ->
                    -- TODO: handle error
                    ( model, Cmd.none )

        PlayerPicked player ->
            case model.state of
                Lobby lobbyState ->
                    ( { model | state = Lobby { lobbyState | selectedPlayer = Just player } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        JoinGameClicked ->
            case model.state of
                Lobby { selectedPlayer } ->
                    case selectedPlayer of
                        Just player ->
                            ( { model | state = Joining player }
                            , Api.postJoinByPlayer (Player.toUrlSegment player) JoinResponse
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartGameClicked ->
            case model.state of
                ReadyToStart connectionStates ->
                    ( { model | state = Starting connectionStates }
                    , Api.postStart StartGameResponse
                    )

                _ ->
                    ( model, Cmd.none )

        StartGameResponse result ->
            -- TODO: handle error. websocket should take care of the happy path
            ( model, Cmd.none )

        GameplayMsg gameplayMsg ->
            case model.state of
                Playing gameplayState ->
                    let
                        ( updatedGamplayState, cmd ) =
                            Gameplay.update gameplayMsg gameplayState
                    in
                    ( { model | state = Playing updatedGamplayState }
                    , Cmd.map GameplayMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    portInfo PortInfoReceived


view : Model -> Html Msg
view model =
    div
        [ css
            [ Css.height (Css.vh 100)
            , Css.width (Css.vw 100)
            , Css.fontFamilies [ "Antic Slab", "serif" ]
            ]
        ]
    <|
        case model.state of
            Loading ->
                [ staticBoard model.boardSvgPath ]

            Lobby lobbyState ->
                [ staticBoard model.boardSvgPath
                , viewColorPickerModal
                    { selectedPlayer = lobbyState.selectedPlayer
                    , freeSlots = lobbyState.connectionStates.freeSlots
                    }
                ]

            Joining player ->
                [ staticBoard model.boardSvgPath
                ]

            WaitingForPlayers connectionStates ->
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = False
                    }
                ]

            ReadyToStart connectionStates ->
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = True
                    }
                ]

            Starting connectionStates ->
                [ sidebarLayout
                    { sidebar =
                        [ viewConnectedPlayers connectionStates.connectedPlayers
                        , viewStartButton False
                        ]
                    , board = [ staticBoard model.boardSvgPath ]
                    }
                ]

            Playing gameState ->
                [ sidebarLayout
                    { sidebar = []
                    , board =
                        [ gameState
                            |> Gameplay.view model.boardSvgPath
                            |> Styled.fromUnstyled
                            |> Styled.map GameplayMsg
                        ]
                    }
                ]


sidebarLayout :
    { sidebar : List (Styled.Html Msg)
    , board : List (Styled.Html Msg)
    }
    -> Html Msg
sidebarLayout { sidebar, board } =
    div [ css [ Css.displayFlex ] ]
        [ div
            [ css
                [ Css.width (px 300)
                , Css.displayFlex
                , Css.flexDirection Css.column
                ]
            ]
            sidebar
        , div [ css [ Css.flexGrow (Css.int 1) ] ]
            board
        ]


viewModal : List (Html Msg) -> Html Msg
viewModal contents =
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
                ]
            ]
            contents
        ]


viewColorPickerModal : { selectedPlayer : Maybe Player, freeSlots : List Player } -> Html Msg
viewColorPickerModal { selectedPlayer, freeSlots } =
    let
        colorOption slot =
            button
                [ Events.onClick (PlayerPicked slot)
                , css
                    [ Css.marginRight (px 5)
                    , Css.backgroundColor Css.unset
                    , Css.border Css.unset
                    , Css.padding4 zero zero (px 2) zero
                    , if selectedPlayer == Just slot then
                        Css.borderBottom3 (px 2) Css.solid (withAlpha 1 (Player.color slot))

                      else
                        Css.border Css.unset
                    ]
                ]
                [ div
                    [ css
                        [ Css.width (px 30)
                        , Css.height (px 30)
                        , Css.borderRadius (px 20)
                        , Css.borderStyle Css.none
                        , Css.lastChild [ Css.marginRight zero ]
                        , Css.backgroundColor
                            (withAlpha
                                (if selectedPlayer == Just slot then
                                    1

                                 else
                                    0.4
                                )
                                (Player.color slot)
                            )
                        ]
                    ]
                    []
                ]
    in
    viewModal
        (if List.isEmpty freeSlots then
            [ text "No quedan lugares 💩" ]

         else
            [ h3 [] [ text "Elegir color" ]
            , div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.alignItems Css.center
                    ]
                ]
                [ div [ css [ Css.marginRight (px 20) ] ] [ text "Color" ]
                , div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.spaceAround
                        , Css.justifyContent Css.spaceAround
                        ]
                    ]
                    (List.map colorOption freeSlots)
                ]
            , Button.view
                { label = "Elegir"
                , isEnabled = selectedPlayer /= Nothing
                , onClick = Just JoinGameClicked
                , css = [ Css.marginTop (px 25) ]
                }
            ]
        )


viewWaitingForPlayersModal : { connectedPlayers : List Player, readyToStart : Bool } -> Html Msg
viewWaitingForPlayersModal { connectedPlayers, readyToStart } =
    let
        viewPlayer player =
            div [ css [ Css.textDecoration3 Css.underline Css.solid (withAlpha 1 (Player.color player)) ] ]
                [ text (Player.label player)
                ]
    in
    viewModal
        [ div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                ]
            ]
            [ h3 [] [ text "En el juego" ]
            , div
                [ css
                    [ Css.minWidth (px 220)
                    , Css.border3 (px 1) Css.solid (Css.hex "#CACACA")
                    , Css.padding2 (px 15) (px 10)
                    ]
                ]
                (List.map viewPlayer connectedPlayers)
            ]
        , viewStartButton readyToStart
        ]


viewConnectedPlayers : List Player -> Styled.Html Msg
viewConnectedPlayers connectedPlayers =
    -- TODO: delete!
    let
        viewPlayer player =
            div
                [ css
                    [ Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.padding2 (px 8) (px 4)
                    , Css.backgroundColor (withAlpha 0.1 (Player.color player))
                    ]
                ]
                [ div
                    [ css
                        [ Css.backgroundColor (withAlpha 1 (Player.color player))
                        , Css.width (px 30)
                        , Css.height (px 30)
                        , Css.borderRadius (px 15)
                        , Css.marginRight (px 10)
                        ]
                    ]
                    []
                , span [] [ text (Player.label player) ]
                ]
    in
    div [ css [ Css.flexGrow (Css.int 1) ] ]
        (if List.isEmpty connectedPlayers then
            [ text "Esperando jugadores" ]

         else
            List.map viewPlayer connectedPlayers
        )


viewStartButton : Bool -> Styled.Html Msg
viewStartButton isEnabled =
    Button.view
        { label = "Empezar juego"
        , isEnabled = isEnabled
        , onClick = Just StartGameClicked
        , css = [ Css.marginTop (px 25) ]
        }


staticBoard : String -> Styled.Html Msg
staticBoard boardSvgPath =
    Styled.fromUnstyled <|
        Board.view
            { svgPath = boardSvgPath
            , onCountryClicked = Nothing
            , onCountryMouseEnter = Nothing
            , onCountryMouseLeave = Nothing
            , highlightedCoutries = []
            }


withAlpha : Float -> { red : Int, green : Int, blue : Int } -> Css.Color
withAlpha alpha { red, green, blue } =
    Css.rgba red green blue alpha
