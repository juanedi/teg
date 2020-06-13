module Gameplay exposing
    ( Msg
    , State
    , init
    , serverUpdate
    , update
    , view
    )

import Api
import Board
import Country exposing (Country)
import Css
import Html exposing (Html)
import Html.Attributes
import Http


type Msg
    = MouseEntered Country
    | MouseLeft Country
    | ClickedCountry Country
    | PaintCountryResponse (Result Http.Error ())


type alias State =
    { game : Api.Game
    , hoveredCountry : Maybe Country
    }


init : Api.Game -> State
init game =
    { game = game
    , hoveredCountry = Nothing
    }


serverUpdate : Api.Game -> State -> State
serverUpdate game state =
    { state | game = game }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        MouseEntered country ->
            ( { state | hoveredCountry = Just country }
            , Cmd.none
            )

        MouseLeft country ->
            ( { state
                | hoveredCountry =
                    case state.hoveredCountry of
                        Nothing ->
                            Nothing

                        Just c ->
                            if c == country then
                                Nothing

                            else
                                Just c
              }
            , Cmd.none
            )

        ClickedCountry country ->
            ( state
            , Api.postPaint ( state.game.identity, country ) PaintCountryResponse
            )

        PaintCountryResponse result ->
            -- TODO: handle error
            ( state, Cmd.none )


view : String -> State -> Html Msg
view boardSvgPath state =
    Board.view
        { svgPath = boardSvgPath
        , onCountryClicked = Just ClickedCountry
        , onCountryMouseEnter = Just MouseEntered
        , onCountryMouseLeave = Just MouseLeft
        , highlightedCoutries =
            List.concat
                [ state.hoveredCountry
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                , state.game.paintedCountries
                    |> List.map Tuple.first
                ]
        }
