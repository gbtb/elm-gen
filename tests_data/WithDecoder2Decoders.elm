module WithDecoder2Decoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import WithDecoder exposing (A(..), dictDecoder)


aDecoder : JD.Decoder A
aDecoder =
    JD.oneOf
        [ JD.field "Foo" (JD.map Foo dictDecoder)
        , JD.field "Bar" (JD.succeed Bar)
        , JD.field "Baz" (JD.succeed Baz)
        ]
