port module Main exposing (main)

import Api
import Board
import Browser
import Browser.Events
import Color exposing (Color)
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
import Ui.Button as Button
import Ui.Theme as Theme


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitSocket
    | Send Api.ClientCommand


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
    | Joining Color
    | WaitingForPlayers Api.ConnectionStates
    | ReadyToStart Api.ConnectionStates
    | Starting Api.ConnectionStates
    | Playing Gameplay.State


type alias LobbyState =
    { name : String
    , selectedColor : Maybe Color
    , hoveredColor : Maybe Color
    , connectionStates : Api.ConnectionStates
    }


type Msg
    = PortInfoReceived Decode.Value
    | NameChanged String
    | ColorHoveredIn Color
    | ColorHoveredOut Color
    | ColorPicked Color
    | JoinGameClicked
    | StartGameClicked
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
    , sendPortCommand (encodePortCommand InitSocket)
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
        InitSocket ->
            Encode.object [ ( "tag", Encode.string "init_socket" ) ]

        Send _ ->
            Encode.object
                [ ( "tag", Encode.string "send" )
                , ( "msg", Encode.null )
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PortInfoReceived jsonValue ->
            case Decode.decodeValue decodePortInfo jsonValue of
                Ok (LobbyStateUpdate connectionStates) ->
                    case model.state of
                        Loading ->
                            ( { model
                                | state =
                                    Lobby
                                        { name = ""
                                        , selectedColor = Nothing
                                        , hoveredColor = Nothing
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

        NameChanged input ->
            ( case model.state of
                Lobby lobbyState ->
                    { model | state = Lobby { lobbyState | name = input } }

                _ ->
                    model
            , Cmd.none
            )

        ColorHoveredIn player ->
            ( case model.state of
                Lobby lobbyState ->
                    { model | state = Lobby { lobbyState | hoveredColor = Just player } }

                _ ->
                    model
            , Cmd.none
            )

        ColorHoveredOut player ->
            ( case model.state of
                Lobby lobbyState ->
                    if lobbyState.hoveredColor == Just player then
                        { model | state = Lobby { lobbyState | hoveredColor = Nothing } }

                    else
                        model

                _ ->
                    model
            , Cmd.none
            )

        ColorPicked player ->
            case model.state of
                Lobby lobbyState ->
                    ( { model | state = Lobby { lobbyState | selectedColor = Just player } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        JoinGameClicked ->
            case model.state of
                Lobby lobbyState ->
                    case validateLobbyInput lobbyState of
                        Just { name, selectedColor } ->
                            ( { model | state = Joining selectedColor }
                            , Send (Api.JoinRoom selectedColor name)
                                |> encodePortCommand
                                |> sendPortCommand
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartGameClicked ->
            case model.state of
                ReadyToStart connectionStates ->
                    ( { model | state = Starting connectionStates }
                    , Send Api.StartGame
                        |> encodePortCommand
                        |> sendPortCommand
                    )

                _ ->
                    ( model, Cmd.none )

        GameplayMsg gameplayMsg ->
            case model.state of
                Playing gameplayState ->
                    let
                        ( updatedGamplayState, effects ) =
                            Gameplay.update gameplayMsg gameplayState
                    in
                    ( { model | state = Playing updatedGamplayState }
                    , effects
                        |> List.map
                            (\effect ->
                                case effect of
                                    Gameplay.CountryPainted country ->
                                        Api.PaintCountry gameplayState.game.identity country
                                            |> Send
                                            |> encodePortCommand
                                            |> sendPortCommand
                            )
                        |> Cmd.batch
                    )

                _ ->
                    ( model, Cmd.none )


validateLobbyInput : LobbyState -> Maybe { name : String, selectedColor : Color }
validateLobbyInput lobbyState =
    -- TODO: check that the name is not repeated
    case lobbyState.selectedColor of
        Nothing ->
            Nothing

        Just color ->
            if String.isEmpty (String.trim lobbyState.name) then
                Nothing

            else
                Just { name = lobbyState.name, selectedColor = color }


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
                , viewLobbyModal lobbyState
                ]

            Joining color ->
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
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = False
                    }
                ]

            Playing gameState ->
                [ gameState
                    |> Gameplay.view model.boardSvgPath
                    |> Styled.fromUnstyled
                    |> Styled.map GameplayMsg
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
            , Css.backgroundColor Theme.backdrop
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


viewLobbyModal : LobbyState -> Html Msg
viewLobbyModal state =
    let
        inputRow id fieldLabel inputMarkup =
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.alignItems Css.center
                    , Css.marginBottom (px 30)
                    , Css.width (Css.pct 100)
                    ]
                ]
                [ label
                    [ Attributes.for id
                    , css [ Css.marginRight (px 30) ]
                    ]
                    [ text fieldLabel ]
                , inputMarkup
                ]
    in
    viewModal
        (if List.isEmpty state.connectionStates.freeSlots then
            [ text "No quedan lugares 💩" ]

         else
            [ h3 [] [ text "Elegir color" ]
            , div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.stretch
                    ]
                ]
                [ inputRow "name-input"
                    "Nombre"
                    (input
                        [ Attributes.id "name-input"
                        , Attributes.type_ "text"
                        , Attributes.autofocus True
                        , Events.onInput NameChanged
                        , css
                            [ Css.width (px 170)
                            , Css.height (px 25)
                            ]
                        ]
                        []
                    )
                , inputRow "color-input"
                    "Color"
                    (viewColorPicker "color-input" state)
                ]
            , Button.view
                { label = "Entrar"
                , isEnabled = validateLobbyInput state /= Nothing
                , onClick = Just JoinGameClicked
                , css = [ Css.marginTop (px 25) ]
                }
            ]
        )


viewColorPicker : String -> LobbyState -> Html Msg
viewColorPicker id state =
    -- TODO: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_radio_role
    let
        viewColorOption slot =
            button
                [ Events.onClick (ColorPicked slot)
                , Events.onMouseEnter (ColorHoveredIn slot)
                , Events.onMouseLeave (ColorHoveredOut slot)
                , css
                    [ Css.marginRight (px 5)
                    , Css.backgroundColor Css.unset
                    , Css.border Css.unset
                    , Css.padding4 zero zero (px 2) zero
                    , Css.borderBottom3 (px 2)
                        Css.solid
                        (if state.selectedColor == Just slot || state.hoveredColor == Just slot then
                            (Color.theme slot).solid

                         else
                            Theme.white
                        )
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
                            ((if state.selectedColor == Just slot then
                                .solid

                              else
                                .light
                             )
                                (Color.theme slot)
                            )
                        ]
                    ]
                    []
                ]
    in
    div
        [ Attributes.id "color-input"
        , css
            [ Css.displayFlex
            , Css.alignItems Css.spaceAround
            , Css.justifyContent Css.spaceAround
            ]
        ]
        -- TODO: show disabled buttons for taken options
        (List.map viewColorOption state.connectionStates.freeSlots)


viewWaitingForPlayersModal : { connectedPlayers : List ( Color, String ), readyToStart : Bool } -> Html Msg
viewWaitingForPlayersModal { connectedPlayers, readyToStart } =
    let
        viewPlayer ( color, name ) =
            div [ css [ Css.textDecoration3 Css.underline Css.solid (Color.theme color).solid ] ]
                [ text name
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
        , Button.view
            { label = "Empezar juego"
            , isEnabled = readyToStart
            , onClick = Just StartGameClicked
            , css = [ Css.marginTop (px 25) ]
            }
        ]


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
