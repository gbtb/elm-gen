module WithDefaultRecordDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithDefaultRecord exposing (A, R)


aDecoder : JD.Decoder A
aDecoder =
    JD.decode A
        |> JD.optional "r" rDecoder { i = 1 }


rDecoder : JD.Decoder R
rDecoder =
    JD.decode R
        |> JD.required "i" JD.int
