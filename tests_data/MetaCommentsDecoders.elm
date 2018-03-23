module MetaCommentsDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import MetaComments exposing (C(..))


cDecoder : JD.Decoder C
cDecoder =
    JD.oneOf
        [ JD.field "Cons1" (JD.map Cons1 JD.int)
        , JD.field "Cons2" (JD.map3 Cons2 (JD.index 0 JD.string) (JD.index 1 JD.int) (JD.index 2 JD.float))
        ]
