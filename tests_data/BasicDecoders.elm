module BasicDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Basic exposing (Basic)


basicDecoder : JD.Decoder Basic
basicDecoder =
    JD.decode Basic
        |> JD.required "a" JD.int
        |> JD.required "b" JD.float
        |> JD.required "c" JD.string
        |> JD.required "d"
            (JD.oneOf
                [ JD.null Nothing
                , JD.map Just JD.int
                ]
            )
