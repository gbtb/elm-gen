module ModelDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Model exposing (GenCommand(..))


genCommandDecoder : JD.Decoder GenCommand
genCommandDecoder =
    JD.oneOf
        [ JD.field "Decoders" (JD.succeed Decoders)
        , JD.field "Encoders" (JD.succeed Encoders)
        , JD.field "DecodersAndEncoders" (JD.succeed DecodersAndEncoders)
        ]
