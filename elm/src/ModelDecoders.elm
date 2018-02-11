module ModelDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Model exposing (GenCommand(..), InputInfo)


genCommandDecoder : JD.Decoder GenCommand
genCommandDecoder =
    JD.oneOf
        [ JD.field "Decoders" (JD.succeed Decoders)
        , JD.field "Encoders" (JD.succeed Encoders)
        , JD.field "DecodersAndEncoders" (JD.succeed DecodersAndEncoders)
        ]


inputInfoDecoder : JD.Decoder InputInfo
inputInfoDecoder =
    JD.decode InputInfo
        |> JD.required "fileContents" JD.string
        |> JD.required "fileNames" (JD.list JD.string)
        |> JD.required "genCommand" genCommandDecoder
