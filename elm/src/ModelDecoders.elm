module ModelDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Config exposing (JsonModulesImports, NameModification, ProvidedNameModification(..), UnionTypeGeneratorFeature(..))
import Model exposing (Config, GenCommand(..), InputInfo, MetaComment(..))


configDecoder : JD.Decoder Config
configDecoder =
    JD.decode Config
        |> JD.optional "genCommand" (maybeDecoder genCommandDecoder) Nothing
        |> JD.optional "encodersName" nameModificationDecoder
            { prefix = ""
            , suffix = "Encoder"
            , providedName = DontTouch
            }
        |> JD.optional "decodersName" nameModificationDecoder
            { prefix = ""
            , suffix = "Decoder"
            , providedName = DontTouch
            }
        |> JD.optional "outputFileName" nameModificationDecoder
            { prefix = ""
            , suffix = ""
            , providedName = DontTouch
            }
        |> JD.optional "jsonModulesImports" jsonModulesImportsDecoder
            { decode = Replace "JD"
            , encode = Replace "JE"
            }
        |> JD.optional "unionTypeGeneratorFeatures" (JD.list unionTypeGeneratorFeatureDecoder) []


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
        |> JD.required "rootDir" JD.string
        |> JD.required "genCommand" genCommandDecoder


jsonModulesImportsDecoder : JD.Decoder JsonModulesImports
jsonModulesImportsDecoder =
    JD.decode JsonModulesImports
        |> JD.required "decode" providedNameModificationDecoder
        |> JD.required "encode" providedNameModificationDecoder


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


metaCommentDecoder : JD.Decoder MetaComment
metaCommentDecoder =
    JD.oneOf
        [ JD.field "Ignore" (JD.succeed Ignore)
        , JD.field "DefaultValue" (JD.succeed DefaultValue)
        , JD.field "NoDeclaration" (JD.succeed NoDeclaration)
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
        ]


unionTypeGeneratorFeatureDecoder : JD.Decoder UnionTypeGeneratorFeature
unionTypeGeneratorFeatureDecoder =
    JD.oneOf
        [ JD.field "TrivialString" (JD.succeed TrivialString)
        , JD.field "TrivialObject" (JD.succeed TrivialObject)
        , JD.field "DefaultConstructor" (JD.succeed DefaultConstructor)
        ]
