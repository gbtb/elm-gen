module WithDefaultValuesDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import WithDefaultValues exposing (A(..), R)


aDecoder : JD.Decoder A
aDecoder =
    JD.oneOf
        [ JD.field "Trivial" (JD.succeed Trivial)
        , JD.field "B" (JD.map B JD.int)
        , JD.field "C" (JD.map C JD.string)
        , JD.succeed (B 3)
        ]


rDecoder : JD.Decoder R
rDecoder =
    JD.succeed R
        |> JD.required "a" JD.int
        |> JD.optional "b" (JD.list JD.string) [ "a" ]
