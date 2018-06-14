module WithDecoderDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithDecoder exposing (A(..), R(..), aDecoder)


rDecoder : JD.Decoder R
rDecoder =
    JD.oneOf [ JD.field "Cons1" (JD.map Cons1 aDecoder) ]


aEncoder : A -> JE.Value
aEncoder value =
    case value of
        Foo ->
            JE.object [ ( "Foo", JE.null ) ]

        Bar ->
            JE.object [ ( "Bar", JE.null ) ]

        Baz ->
            JE.object [ ( "Baz", JE.null ) ]


rEncoder : R -> JE.Value
rEncoder value =
    case value of
        Cons1 v1 ->
            JE.object [ ( "Cons1", aEncoder v1 ) ]
