port module Main exposing (main)

import Api
import Board
import Browser
import Browser.Events
import Country exposing (Country)
import Css
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Gameplay
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
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
    = LobbyStateUpdate (List Player)
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
      Lobby (List Player)
    | Joining Player
    | WaitingForPlayers (List Player)
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
        , view = view >> Html.toUnstyled
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
                Ok (LobbyStateUpdate freeSlots) ->
                    case model.state of
                        Loading ->
                            ( { model | state = Lobby freeSlots }
                            , Cmd.none
                            )

                        Lobby _ ->
                            ( { model | state = Lobby freeSlots }
                            , Cmd.none
                            )

                        WaitingForPlayers _ ->
                            ( { model | state = WaitingForPlayers freeSlots }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Ok (GameStateUpdate room) ->
                    case room of
                        Api.WaitingForPlayers freeSlots ->
                            ( { model | state = WaitingForPlayers freeSlots }
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
            Html.text "Joining the game"

        Lobby freeSlots ->
            { sidebar = viewLobbySidebar freeSlots
            , board = [ staticBoard model.boardSvgPath ]
            }
                |> sidebarLayout
                |> Element.layout []
                |> Html.fromUnstyled

        Joining player ->
            Html.text ("Joining as " ++ Player.toUrlSegment player)

        WaitingForPlayers freeSlots ->
            Html.div []
                [ Html.text "Waiting for other players to join: "
                , Html.ul []
                    (List.map
                        (\slot ->
                            Html.li [] [ Html.text (Player.toUrlSegment slot) ]
                        )
                        freeSlots
                    )
                ]

        Playing gameState ->
            Html.map
                GameplayMsg
                (Gameplay.view model.boardSvgPath gameState)


sidebarLayout :
    { sidebar : List (Element Msg)
    , board : List (Element Msg)
    }
    -> Element Msg
sidebarLayout { sidebar, board } =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.column
            [ Element.width (Element.px 300)
            , Element.spacing 30
            ]
            sidebar
        , Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.centerX
            , Element.Border.glow (Element.rgb 0 0 0) 0.3
            ]
            board
        ]


viewLobbySidebar : List Player -> List (Element Msg)
viewLobbySidebar freeSlots =
    [ Element.el
        [ Element.centerX ]
        (Element.text "Elegir color")
    , Element.row
        [ Element.centerX
        , Element.spacing 10
        ]
        (List.map
            (\slot ->
                Element.Input.button
                    [ Element.paddingXY 20 10
                    , Element.Background.color (Player.color slot |> withAlpha 0.1)
                    , Element.Border.color (Player.color slot |> withAlpha 0.3)
                    , Element.Border.width 1
                    , Element.Border.rounded 2
                    , Element.mouseOver
                        [ Element.Background.color (Player.color slot |> withAlpha 0.2)
                        ]
                    ]
                    { onPress = Just (PlayerPicked slot)
                    , label = Element.text (Player.label slot)
                    }
            )
            freeSlots
        )
    ]


staticBoard : String -> Element Msg
staticBoard boardSvgPath =
    Element.html <|
        Board.view
            { svgPath = boardSvgPath
            , onCountryClicked = Nothing
            , onCountryMouseEnter = Nothing
            , onCountryMouseLeave = Nothing
            , highlightedCoutries = []
            }


withAlpha : Float -> { red : Float, green : Float, blue : Float } -> Element.Color
withAlpha alpha rgb =
    Element.fromRgb
        { red = rgb.red
        , green = rgb.green
        , blue = rgb.blue
        , alpha = alpha
        }
