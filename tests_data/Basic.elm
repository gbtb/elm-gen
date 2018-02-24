module Basic exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


type alias Basic =
    { a : Int
    , b : Float
    , c : String
    , d : Maybe Int
    }
