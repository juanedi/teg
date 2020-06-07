port module Main exposing (main)

import Api
import Browser
import Country exposing (Country)
import Css
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
    { boardSvgPath : String
    }


type alias Model =
    { boardSvgPath : String
    , state : State
    }


type State
    = Loading
    | -- user is deciding which player/color to use
      Lobby (List Player)
    | Joining Player
    | WaitingForPlayers
    | Playing Gameplay.State


type Msg
    = JoinResponse (Result Http.Error ())
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

                        _ ->
                            ( model, Cmd.none )

                Ok (GameStateUpdate room) ->
                    case room of
                        Api.WaitingForPlayers ->
                            ( { model | state = WaitingForPlayers }
                            , Cmd.none
                            )

                        Api.Started game ->
                            ( { model
                                | state =
                                    case model.state of
                                        WaitingForPlayers ->
                                            Playing (Gameplay.init game)

                                        Playing gameplayState ->
                                            Playing (Gameplay.serverUpdate game gameplayState)

                                        _ ->
                                            model.state
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
    portInfo PortInfoReceived


view : Model -> Html Msg
view model =
    case model.state of
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

        WaitingForPlayers ->
            Html.text "Waiting for other players to join"

        Playing gameState ->
            Html.map
                GameplayMsg
                (Gameplay.view model.boardSvgPath gameState)
