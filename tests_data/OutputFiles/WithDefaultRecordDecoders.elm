module WithDefaultRecordDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import WithDefaultRecord exposing (A, R)


aDecoder : JD.Decoder A
aDecoder =
    JD.succeed A
        |> JD.optional "r" rDecoder { i = 1 }


rDecoder : JD.Decoder R
rDecoder =
    JD.succeed R
        |> JD.required "i" JD.int
