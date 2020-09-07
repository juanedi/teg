module Gameplay exposing
    ( Effect(..)
    , Msg
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


type Effect
    = CountryPainted Country


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


update : Msg -> State -> ( State, List Effect )
update msg state =
    case msg of
        MouseEntered country ->
            ( { state | hoveredCountry = Just country }
            , []
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
            , []
            )

        ClickedCountry country ->
            ( state, [ CountryPainted country ] )


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
