module ConfigDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Config exposing (Config, NameModification, ProvidedNameModification(..), UnionTypeGenerator(..))
import Model exposing (GenCommand(..))


configDecoder : JD.Decoder Config
configDecoder =
    JD.decode Config
        |> JD.required "genCommand" (maybeDecoder genCommandDecoder)
        |> JD.required "encodersName" nameModificationDecoder
        |> JD.required "decodersName" nameModificationDecoder
        |> JD.required "outputFileName" nameModificationDecoder
        |> JD.required "unionTypeGenerator" unionTypeGeneratorDecoder


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


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


nameModificationDecoder : JD.Decoder NameModification
nameModificationDecoder =
    JD.decode NameModification
        |> JD.required "prefix" JD.string
        |> JD.required "suffix" JD.string
        |> JD.required "providedName" providedNameModificationDecoder


providedNameModificationDecoder : JD.Decoder ProvidedNameModification
providedNameModificationDecoder =
    JD.oneOf
        [ JD.field "DontTouch" (JD.succeed DontTouch)
        , JD.field "Replace" (JD.map Replace JD.string)
        ]


unionTypeGeneratorDecoder : JD.Decoder UnionTypeGenerator
unionTypeGeneratorDecoder =
    JD.oneOf
        [ JD.field "TrivialString" (JD.succeed TrivialString)
        , JD.field "TrivialObject" (JD.succeed TrivialObject)
        ]
