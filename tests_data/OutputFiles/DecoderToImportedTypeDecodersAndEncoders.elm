module DecoderToImportedTypeDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Basic exposing (Basic, timeDecoder, timeEncoder)


basicDecoder : JD.Decoder Basic
basicDecoder =
    JD.succeed Basic
        |> JD.required "a" JD.int
        |> JD.required "b" JD.float
        |> JD.required "c" timeDecoder
        |> JD.required "d" (maybeDecoder JD.int)


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


basicEncoder : Basic -> JE.Value
basicEncoder value =
    JE.object
        [ ( "a", JE.int value.a )
        , ( "b", JE.float value.b )
        , ( "c", timeEncoder value.c )
        , ( "d", maybeEncoder JE.int value.d )
        ]


maybeEncoder valueEncoder value =
    case value of
        Just value ->
            valueEncoder value

        Nothing ->
            JE.null
