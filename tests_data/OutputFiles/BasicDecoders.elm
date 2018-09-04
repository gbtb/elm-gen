module BasicDecoders exposing (..)

import Basic exposing (Basic)
import Json.Decode as JD
import Json.Decode.Pipeline as JD


basicDecoder : JD.Decoder Basic
basicDecoder =
    JD.succeed Basic
        |> JD.required "a" JD.int
        |> JD.required "b" JD.float
        |> JD.required "c" JD.string
        |> JD.required "d" (maybeDecoder JD.int)


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]
