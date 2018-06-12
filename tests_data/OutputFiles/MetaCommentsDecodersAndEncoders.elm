module MetaCommentsDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import MetaComments exposing (C(..), D)


cDecoder : JD.Decoder C
cDecoder =
    JD.oneOf
        [ JD.field "Cons1" (JD.map Cons1 JD.int)
        , JD.field "Cons2" (JD.map3 Cons2 (JD.index 0 JD.string) (JD.index 1 JD.int) (JD.index 2 JD.float))
        ]


dDecoder =
    JD.decode D
        |> JD.required "foo" JD.int
        |> JD.required "bar" cDecoder


cEncoder : C -> JE.Value
cEncoder value =
    case value of
        Cons1 v1 ->
            JE.object [ ( "Cons1", JE.int v1 ) ]

        Cons2 v1 v2 v3 ->
            JE.object [ ( "Cons2", JE.list [ JE.string v1, JE.int v2, JE.float v3 ] ) ]


dEncoder value =
    JE.object
        [ ( "foo", JE.int value.foo )
        , ( "bar", cEncoder value.bar )
        ]
