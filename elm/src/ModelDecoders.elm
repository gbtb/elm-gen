module ModelDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Config exposing (NameModification, ProvidedNameModification(..), UnionTypeGeneratorFeature(..))
import Model exposing (Config, GenCommand(..), InputInfo)


configDecoder : JD.Decoder Config
configDecoder =
    JD.decode Config
        |> JD.optional "genCommand" (maybeDecoder genCommandDecoder) Nothing
        |> JD.required "encodersName" nameModificationDecoder
        |> JD.required "decodersName" nameModificationDecoder
        |> JD.required "outputFileName" nameModificationDecoder
        |> JD.required "unionTypeGeneratorFeatures" (JD.list unionTypeGeneratorFeatureDecoder)


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
        |> JD.optional "prefix" JD.string ""
        |> JD.optional "suffix" JD.string ""
        |> JD.optional "providedName" providedNameModificationDecoder DontTouch


providedNameModificationDecoder : JD.Decoder ProvidedNameModification
providedNameModificationDecoder =
    JD.oneOf
        [ JD.field "DontTouch" (JD.succeed DontTouch)
        , JD.field "Replace" (JD.map Replace JD.string)
        , JD.succeed DontTouch
        ]


unionTypeGeneratorFeatureDecoder : JD.Decoder UnionTypeGeneratorFeature
unionTypeGeneratorFeatureDecoder =
    JD.oneOf
        [ JD.field "TrivialString" (JD.succeed TrivialString)
        , JD.field "TrivialObject" (JD.succeed TrivialObject)
        , JD.field "DefaultConstructor" (JD.succeed DefaultConstructor)
        ]
