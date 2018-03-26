module WithDecoder exposing (B)

import Json.Decode as JD
import Dict


type A
    = Foo Dict.Dict
    | Bar
    | Baz


dictDecoder : JD.Decoder Dict.Dict
dictDecoder =
    JD.fail "It doesnot matter"
