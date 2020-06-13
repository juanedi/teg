port module Main exposing (main)

import Api
import Board
import Browser
import Browser.Events
import Country exposing (Country)
import Css
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gameplay
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Player exposing (Player)


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitLobbySocket
    | InitGameSocket Player


type PortInfo
    = LobbyStateUpdate Api.ConnectionStates
    | GameStateUpdate Api.Room


type alias Flags =
    { viewport : Viewport
    , boardSvgPath : String
    }


type alias Model =
    { viewport : Viewport
    , boardSvgPath : String
    , state : State
    }


type alias Viewport =
    { width : Int, height : Int }


type State
    = Loading
    | -- user is deciding which player/color to use
      Lobby Api.ConnectionStates
    | Joining Player
    | WaitingForPlayers Api.ConnectionStates
    | Playing Gameplay.State


type Msg
    = ViewportChanged { width : Int, height : Int }
    | JoinResponse (Result Http.Error ())
    | PortInfoReceived Decode.Value
    | PlayerPicked Player
    | GameplayMsg Gameplay.Msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { viewport, boardSvgPath } =
    ( { viewport = viewport
      , boardSvgPath = boardSvgPath
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
        ViewportChanged viewport ->
            ( { model | viewport = viewport }
            , Cmd.none
            )

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
                            ( { model | state = Lobby connectionStates }
                            , Cmd.none
                            )

                        Lobby _ ->
                            ( { model | state = Lobby connectionStates }
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
            ( { model | state = Joining player }
            , Api.postJoinByPlayer (Player.toUrlSegment player) JoinResponse
            )

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
    Sub.batch
        [ portInfo PortInfoReceived
        , Browser.Events.onResize (\w h -> ViewportChanged { width = w, height = h })
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Loading ->
            { viewport = model.viewport
            , sidebar = []
            , board = [ staticBoard model.boardSvgPath ]
            }
                |> sidebarLayout
                |> Element.layout []

        Lobby connectionStates ->
            Element.layout
                [ Element.inFront (viewColorPicker connectionStates.freeSlots) ]
                (sidebarLayout
                    { viewport = model.viewport
                    , sidebar = [ viewConnectedPlayers connectionStates.connectedPlayers ]
                    , board = [ staticBoard model.boardSvgPath ]
                    }
                )

        Joining player ->
            { viewport = model.viewport
            , sidebar = []
            , board = [ staticBoard model.boardSvgPath ]
            }
                |> sidebarLayout
                |> Element.layout []

        WaitingForPlayers connectionStates ->
            { viewport = model.viewport
            , sidebar = [ viewConnectedPlayers connectionStates.connectedPlayers ]
            , board = [ staticBoard model.boardSvgPath ]
            }
                |> sidebarLayout
                |> Element.layout []

        Playing gameState ->
            { viewport = model.viewport
            , sidebar = []
            , board =
                [ gameState
                    |> Gameplay.view model.boardSvgPath
                    |> Element.html
                    |> Element.map GameplayMsg
                ]
            }
                |> sidebarLayout
                |> Element.layout []


sidebarLayout :
    { viewport : Viewport
    , sidebar : List (Element Msg)
    , board : List (Element Msg)
    }
    -> Element Msg
sidebarLayout { viewport, sidebar, board } =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.column
            [ Element.width (Element.px 250)
            , Element.height (Element.fillPortion 2)
            , Element.spacing 30
            , Font.size 14
            ]
            sidebar
        , Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.centerX
            , Border.glow (Element.rgb 0 0 0) 0.3
            ]
            board
        ]


viewColorPicker : List Player -> Element Msg
viewColorPicker freeSlots =
    viewModal
        (if List.isEmpty freeSlots then
            [ Element.el
                [ Element.centerX
                , Font.size 16
                , Font.bold
                ]
                (Element.text "No quedan lugares 💩")
            ]

         else
            [ Element.el
                [ Element.centerX
                , Font.size 16
                , Font.bold
                ]
                (Element.text "Elegir color")
            , Element.row
                [ Element.centerX
                , Element.spacing 10
                ]
                (List.map
                    (\slot ->
                        Input.button
                            [ Element.width (Element.px 40)
                            , Element.height (Element.px 40)
                            , Background.color (Player.color slot |> withAlpha 0.1)
                            , Border.color (Player.color slot |> withAlpha 0.3)
                            , Border.width 1
                            , Border.rounded 20
                            , Font.size 12
                            , Element.mouseOver
                                [ Background.color (Player.color slot |> withAlpha 0.4)
                                ]
                            ]
                            { onPress = Just (PlayerPicked slot)
                            , label = Element.text ""
                            }
                    )
                    freeSlots
                )
            ]
        )


viewModal : List (Element Msg) -> Element Msg
viewModal content =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.width (Element.shrink |> Element.minimum 200)
        , Element.padding 20
        , Element.spacing 20
        , Background.color (Element.rgb255 255 255 255)
        , Border.rounded 4
        , Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 3
            , color = Element.rgb 0 0 0
            }
        ]
        content


viewConnectedPlayers : List Player -> Element Msg
viewConnectedPlayers connectedPlayers =
    let
        viewPlayer player =
            Element.row
                [ Element.spacing 10
                , Element.padding 10
                , Element.width Element.fill
                , Background.color (Player.color player |> withAlpha 0.1)
                ]
                [ Element.el
                    [ Element.height (Element.px 10)
                    , Element.width (Element.px 10)
                    , Background.color (Player.color player |> withAlpha 1)
                    , Border.rounded 5
                    ]
                    (Element.text "")
                , Element.text (Player.label player)
                ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 1)
        , Border.glow (Element.rgb 0 0 0) 0.3
        ]
        (if List.isEmpty connectedPlayers then
            [ Element.column
                [ Font.size 13
                , Element.centerX
                , Element.centerY
                , Font.color (Element.rgb255 150 150 150)
                ]
                [ Element.text "Esperado jugadores" ]
            ]

         else
            List.map viewPlayer connectedPlayers
        )


staticBoard : String -> Element Msg
staticBoard boardSvgPath =
    Element.el
        [ Element.alpha 0.4
        , Element.height Element.fill
        , Element.width Element.fill
        ]
        (Element.html <|
            Board.view
                { svgPath = boardSvgPath
                , onCountryClicked = Nothing
                , onCountryMouseEnter = Nothing
                , onCountryMouseLeave = Nothing
                , highlightedCoutries = []
                }
        )


withAlpha : Float -> { red : Float, green : Float, blue : Float } -> Element.Color
withAlpha alpha rgb =
    Element.fromRgb
        { red = rgb.red
        , green = rgb.green
        , blue = rgb.blue
        , alpha = alpha
        }
