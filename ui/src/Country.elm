module Country exposing
    ( Country
    , decoder
    , svgId
    )

import Api
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Country =
    Api.Country


decoder : Decoder Country
decoder =
    Api.jsonDecCountry


{-| Returns the id used to identify the country in the board's svg. this is also
the label we use to serialize the country to/from JSON.
-}
svgId : Country -> String
svgId country =
    country
        |> Api.jsonEncCountry
        |> Encode.encode 0
        |> String.dropLeft 1
        |> String.dropRight 1
