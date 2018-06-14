module WithEncoder exposing (..)

import Json.Encode as JE


type A
    = Foo
    | Bar
    | Baz


type R
    = Cons1 A


annEncoder : A -> JE.Value
annEncoder value =
    JE.null
