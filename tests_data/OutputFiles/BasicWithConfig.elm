module BasicWithConfig exposing (..)

import Basic exposing (Basic)
import Json.Decode as JD
import Json.Decode.Pipeline as JD


basicdecoder : JD.Decoder Basic
basicdecoder =
    JD.succeed Basic
        |> JD.required "a" JD.int
        |> JD.required "b" JD.float
        |> JD.required "c" JD.string
        |> JD.required "d" (maybedecoder JD.int)


maybedecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]
