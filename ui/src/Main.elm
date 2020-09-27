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
import Ui.Modal as Modal
import Ui.Theme as Theme


port sendPortCommand : Value -> Cmd msg


port portInfo : (Value -> msg) -> Sub msg


type PortCommand
    = InitSocket String
    | Send Api.ClientCommand
    | Copy String


type alias Model =
    { boardSvgPath : String
    , roomUrl : Url
    , state : State
    }


type alias Url =
    String


type State
    = Loading
    | Lobby LobbyState
    | Joining Color
    | WaitingForPlayers Api.ConnectionStates
    | ReadyToStart Api.ConnectionStates
    | Starting Api.ConnectionStates
    | Playing Gameplay.State
    | -- the user is in the game, but others have disconnected
      Paused (List ( Color, String ))
    | -- the user is not part of the game, but the game is paused because a
      -- player left so they are offered to pick one of the free spots
      Reconnecting (List ( Color, String ))


type alias LobbyState =
    { name : String
    , selectedColor : Maybe Color
    , connectionStates : Api.ConnectionStates
    }


type Msg
    = PortInfoReceived Decode.Value
    | NameChanged String
    | ColorPicked Color
    | CopyRoomUrl
    | IgnoreRoomUrlInput
    | JoinGameClicked
    | ReconnectClicked Color String
    | StartGameClicked
    | GameplayMsg Gameplay.Msg


main : Program Api.Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : Api.Flags -> ( Model, Cmd Msg )
init flags =
    ( { boardSvgPath = flags.boardSvgPath
      , roomUrl = flags.roomUrl
      , state = Loading
      }
    , InitSocket flags.websocketUrl
        |> encodePortCommand
        |> sendPortCommand
    )


encodePortCommand : PortCommand -> Value
encodePortCommand cmd =
    case cmd of
        InitSocket url ->
            Encode.object
                [ ( "tag", Encode.string "init_socket" )
                , ( "url", Encode.string url )
                ]

        Send socketMsg ->
            Encode.object
                [ ( "tag", Encode.string "send" )
                , ( "msg", Api.jsonEncClientCommand socketMsg )
                ]

        Copy text ->
            Encode.object
                [ ( "tag", Encode.string "copy" )
                , ( "containerId", Encode.string text )
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PortInfoReceived jsonValue ->
            case Decode.decodeValue Api.jsonDecDataForClient jsonValue of
                Ok (Api.LobbyUpdate (Api.Reconnecting missingPlayers)) ->
                    ( { model | state = Reconnecting missingPlayers }
                    , Cmd.none
                    )

                Ok (Api.LobbyUpdate (Api.Lobby connectionStates)) ->
                    case model.state of
                        Loading ->
                            ( { model
                                | state =
                                    Lobby
                                        { name = ""
                                        , selectedColor = Nothing
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

                Ok (Api.RoomUpdate room) ->
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

                        Api.Paused missingPlayers ->
                            ( { model | state = Paused missingPlayers }
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

        ColorPicked player ->
            case model.state of
                Lobby lobbyState ->
                    ( { model | state = Lobby { lobbyState | selectedColor = Just player } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CopyRoomUrl ->
            ( model
            , Copy roomUrlContainerId
                |> encodePortCommand
                |> sendPortCommand
            )

        IgnoreRoomUrlInput ->
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

        ReconnectClicked color name ->
            case model.state of
                Reconnecting _ ->
                    ( { model | state = Joining color }
                    , Send (Api.JoinRoom color name)
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
                , viewLobbyModal model.roomUrl lobbyState
                ]

            Joining color ->
                [ staticBoard model.boardSvgPath
                ]

            WaitingForPlayers connectionStates ->
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = False
                    , roomUrl = model.roomUrl
                    }
                ]

            ReadyToStart connectionStates ->
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = True
                    , roomUrl = model.roomUrl
                    }
                ]

            Starting connectionStates ->
                [ staticBoard model.boardSvgPath
                , viewWaitingForPlayersModal
                    { connectedPlayers = connectionStates.connectedPlayers
                    , readyToStart = False
                    , roomUrl = model.roomUrl
                    }
                ]

            Playing gameState ->
                [ gameState
                    |> Gameplay.view model.boardSvgPath
                    |> Styled.fromUnstyled
                    |> Styled.map GameplayMsg
                ]

            Paused missingPlayers ->
                [ staticBoard model.boardSvgPath
                , viewPausedModal missingPlayers
                ]

            Reconnecting missingPlayers ->
                [ staticBoard model.boardSvgPath
                , viewReconnectModal missingPlayers
                ]


viewLobbyModal : Url -> LobbyState -> Html Msg
viewLobbyModal roomUrl state =
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
    Modal.view
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
            , Button.button
                { label = "Entrar"
                , isEnabled = validateLobbyInput state /= Nothing
                , onClick = Just JoinGameClicked
                , size = Button.Large
                , css = []
                }
                []
            , viewCopyRoomUrl roomUrl
            ]
        )


roomUrlContainerId : String
roomUrlContainerId =
    "room-url"


viewCopyRoomUrl : Url -> Html Msg
viewCopyRoomUrl roomUrl =
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.marginTop (px 25)
            ]
        ]
        [ h5 [ css [ Css.margin4 zero zero (px 5) zero ] ]
            [ text "Compartir link" ]
        , div [ css [ Css.displayFlex ] ]
            [ input
                [ Attributes.type_ "text"
                , Attributes.value roomUrl
                , Attributes.id roomUrlContainerId
                , Events.onInput (\_ -> IgnoreRoomUrlInput)
                , css [ Css.flexGrow (Css.int 1) ]
                ]
                []
            , Button.button
                { label = "Copiar"
                , onClick = Just CopyRoomUrl
                , isEnabled = True
                , size = Button.Small
                , css =
                    [ Css.borderRadius4 zero (px 6) (px 6) zero
                    ]
                }
                []
            ]
        ]


viewColorPicker : String -> LobbyState -> Html Msg
viewColorPicker id state =
    -- TODO: https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_radio_role
    let
        border color =
            Css.borderBottom3 (px 2) Css.solid color

        viewColorOption slot =
            button
                [ Events.onClick (ColorPicked slot)
                , css
                    [ Css.marginRight (px 5)
                    , Css.backgroundColor Css.unset
                    , Css.border Css.unset
                    , Css.padding4 zero zero (px 2) zero
                    , border
                        (if state.selectedColor == Just slot then
                            (Color.theme slot).solid

                         else
                            Theme.white
                        )
                    , Css.hover
                        [ border (Color.theme slot).solid
                        ]
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


viewUnderlinedPlayer : ( Color, String ) -> Html Msg
viewUnderlinedPlayer ( color, name ) =
    div
        [ css
            [ Css.textDecoration Css.underline
            , -- using this instead of textDecorationN for compatibility with Safari
              Css.property "text-decoration-color" (Color.theme color).solid.value
            ]
        ]
        [ text name
        ]


viewWaitingForPlayersModal : { connectedPlayers : List ( Color, String ), readyToStart : Bool, roomUrl : Url } -> Html Msg
viewWaitingForPlayersModal { connectedPlayers, readyToStart, roomUrl } =
    Modal.view
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
                (List.map viewUnderlinedPlayer connectedPlayers)
            ]
        , Button.button
            { label =
                if readyToStart then
                    "Empezar juego"

                else
                    "Esperando jugadores"
            , isEnabled = readyToStart
            , onClick = Just StartGameClicked
            , size = Button.Large
            , css = [ Css.marginTop (px 25) ]
            }
            []
        , viewCopyRoomUrl roomUrl
        ]


viewPausedModal : List ( Color, String ) -> Html Msg
viewPausedModal missingPlayers =
    Modal.view
        [ text "Esperando jugadores"
        , ul
            [ css
                [ Css.margin2 (px 10) zero
                , Css.property "list-style-type" "unset"
                , Css.property "padding-inline-start" "20px"
                , Css.width (Css.pct 100)
                ]
            ]
            (List.map viewUnderlinedPlayer missingPlayers)
        ]


viewReconnectModal : List ( Color, String ) -> Html Msg
viewReconnectModal missingPlayers =
    Modal.view
        [ text "Volver al juego"
        , ul
            [ css
                [ Css.margin4 (px 20) zero zero zero
                , Css.listStyle Css.none
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.stretch
                , Css.padding Css.zero
                , Css.width (Css.pct 100)
                ]
            ]
            (List.map
                (\( color, name ) ->
                    li [ css [ Css.marginBottom (px 10) ] ]
                        [ button
                            [ css
                                [ Css.backgroundColor Theme.white
                                , Css.border3 (px 1) Css.solid Theme.grey
                                , Css.property "font" "unset"
                                , Css.padding (px 10)
                                , Css.borderRadius (px 3)
                                , Css.width (Css.pct 100)
                                , Css.cursor Css.pointer
                                , Css.hover
                                    [ Css.backgroundColor Theme.greyLight
                                    ]
                                ]
                            , Events.onClick (ReconnectClicked color name)
                            ]
                            [ viewUnderlinedPlayer ( color, name )
                            ]
                        ]
                )
                missingPlayers
            )
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
