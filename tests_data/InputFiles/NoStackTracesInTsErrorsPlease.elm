module Basic exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Time exposing (Time)


type alias Basic =
    { a : Int
    , b : Float
    , c : Time
    , d : Maybe Int
    }
