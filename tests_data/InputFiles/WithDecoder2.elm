module WithDecoder exposing (B)

import Json.Decode as JD
import Dict


type alias SomeDict =
    Dict.Dict String Int


type A
    = Foo SomeDict
    | Bar
    | Baz


dictDecoder : JD.Decoder SomeDict
dictDecoder =
    JD.fail "It doesnot matter"
