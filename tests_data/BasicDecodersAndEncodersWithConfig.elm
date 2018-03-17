module BasicDecodersAndEncodersWithConfig exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Basic exposing (Basic)


basicdecoder : JD.Decoder Basic
basicdecoder =
    JD.decode Basic
        |> JD.required "a" JD.int
        |> JD.required "b" JD.float
        |> JD.required "c" JD.string
        |> JD.required "d" (maybedecoder JD.int)


maybedecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


encodebasic : Basic -> JE.Value
encodebasic value =
    JE.object
        [ ( "a", JE.int value.a )
        , ( "b", JE.float value.b )
        , ( "c", JE.string value.c )
        , ( "d", encodemaybe JE.int value.d )
        ]


encodemaybe valueEncoder value =
    case value of
        Just value ->
            valueEncoder value

        Nothing ->
            JE.null
