module WithEncoderDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithEncoder exposing (A(..), R(..), annEncoder)


aDecoder : JD.Decoder A
aDecoder =
    JD.oneOf
        [ JD.field "Foo" (JD.succeed Foo)
        , JD.field "Bar" (JD.succeed Bar)
        , JD.field "Baz" (JD.succeed Baz)
        ]


rDecoder : JD.Decoder R
rDecoder =
    JD.oneOf [ JD.field "Cons1" (JD.map Cons1 aDecoder) ]


rEncoder : R -> JE.Value
rEncoder value =
    case value of
        Cons1 v1 ->
            JE.object [ ( "Cons1", annEncoder v1 ) ]
