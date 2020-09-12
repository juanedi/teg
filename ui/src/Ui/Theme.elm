module Ui.Theme exposing (..)

import Css exposing (Color)



-- COMMON UI THEME COLORS


white : Color
white =
    Css.hex "#FFFFFF"


grey : Color
grey =
    Css.hex "#CACACA"


greyLight : Color
greyLight =
    Css.rgb 240 240 240


backdrop : Color
backdrop =
    Css.rgba 0 0 0 0.2



--  PLAYER COLORS


blueSolid : Color
blueSolid =
    Css.rgba 0 0 255 1


blueLight : Color
blueLight =
    Css.rgba 0 0 255 0.4


redSolid : Color
redSolid =
    Css.rgba 255 0 0 1.0


redLight : Color
redLight =
    Css.rgba 255 0 0 0.4


blackSolid : Color
blackSolid =
    Css.rgba 0 0 0 1


blackLight : Color
blackLight =
    Css.rgba 0 0 0 0.4


yellowSolid : Color
yellowSolid =
    Css.rgba 255 234 0 1


yellowLight : Color
yellowLight =
    Css.rgba 255 234 0 0.4


greenSolid : Color
greenSolid =
    Css.rgba 0 255 0 1


greenLight : Color
greenLight =
    Css.rgba 0 255 0 0.4


magentaSolid : Color
magentaSolid =
    Css.rgba 255 0 255 1


magentaLight : Color
magentaLight =
    Css.rgba 255 0 255 0.4
