module ModuleDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Nested.Module exposing (T(..))


tDecoder : JD.Decoder T
tDecoder =
    JD.oneOf
        [ JD.field "A" (JD.succeed A)
        , JD.field "B" (JD.map B JD.string)
        , JD.field "C" (JD.map C JD.float)
        ]
