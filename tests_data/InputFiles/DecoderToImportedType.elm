module Basic exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Time exposing (Time)


timeDecoder : JD.Decoder Time
timeDecoder =
    JD.fail "111"


timeEncoder : Time -> JE.Value
timeEncoder t =
    JE.null


type alias Basic =
    { a : Int
    , b : Float
    , c : Time
    , d : Maybe Int
    }
