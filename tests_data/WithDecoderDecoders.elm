module WithDecoderDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithDecoder exposing (aDecoder, R)


rDecoder : JD.Decoder R
rDecoder =
    JD.oneOf
        [ JD.field "Cons1" aDecoder
        ]
