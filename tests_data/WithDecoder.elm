module WithDecoder exposing (B)

import Json.Decode as JD


type A
    = Foo
    | Bar
    | Baz


type R
    = Cons1 A


aDecoder : JD.Decoder A
aDecoder =
    JD.fail "It doesnot matter"
