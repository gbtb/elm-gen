module WithBoth exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


type alias Basic =
    { a : Int
    , b : Float
    , c : String
    , d : Maybe Int
    }


basicDecode : JD.Decoder Basic
basicDecode =
    JD.fail "idc"


basicEncoder : Basic -> JE.Value
basicEncoder value =
    JE.null


type DependentType
    = A
    | B Basic
    | C Record


type alias Record =
    { field1 : List Float
    , field2 : Basic
    }
