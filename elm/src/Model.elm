module Model exposing (..)

import Set
import Dict
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Config exposing (NameModification, UnionTypeGeneratorFeature(..), ProvidedNameModification(..), JsonModulesImports)


type GenCommand
    = Decoders
    | Encoders
    | DecodersAndEncoders


type alias InputInfo =
    { fileContents : String
    , fileNames : List String
    , rootDir : String
    , genCommand : GenCommand
    }


type MetaComment
    = Ignore
    | DefaultValue
    | NoDeclaration
    | FieldNameConversion
    | FieldNameConversionApplication String


{-| //Ignore
-}
type alias TypeSet =
    Set.Set TypeName


{-| //Ignore
-}
type alias TypeName =
    List String


{-| //Ignore
-}
type alias Model =
    { inputFileContent : String
    , outputFileName : String
    , rootDir : String
    , genCommand : GenCommand
    , parsedStatements : List Statement
    , newlyParsedStatements : List Statement
    , unknownTypes : TypeSet
    , typesDict : Dict.Dict TypeName Statement
    , dependencies : ( TypeSet, Dict.Dict TypeName TypeSet )
    , importsDict : Dict.Dict (List String) TypeSet
    , moduleDeclaration : Statement
    , providedDecoders : Dict.Dict TypeName String
    , providedEncoders : Dict.Dict TypeName String
    , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
    , defaultUnionValues : Dict.Dict TypeName Expression
    , fieldNameMapping : Dict.Dict String (Dict.Dict String String)
    , fieldNameMappingApplications : Dict.Dict TypeName String
    , dontDeclareTypes : Set.Set TypeName
    , generatedDecoders : List (List Statement)
    , generatedEncoders : List (List Statement)
    , config : Config
    }


initModel =
    { inputFileContent = ""
    , genCommand = Decoders
    , outputFileName = ""
    , rootDir = ""
    , parsedStatements = []
    , newlyParsedStatements = []
    , unknownTypes = Set.empty
    , typesDict = Dict.empty
    , dependencies = ( Set.empty, Dict.empty )
    , importsDict = Dict.empty
    , moduleDeclaration = Comment "Init placeholder"
    , providedDecoders = Dict.empty
    , providedEncoders = Dict.empty
    , defaultRecordValues = Dict.empty
    , defaultUnionValues = Dict.empty
    , fieldNameMapping = Dict.empty
    , fieldNameMappingApplications = Dict.empty
    , dontDeclareTypes = Set.empty
    , generatedDecoders = []
    , generatedEncoders = []
    , config = initConfig
    }


type alias Config =
    { genCommand : Maybe GenCommand
    , encodersName : NameModification
    , decodersName : NameModification
    , outputFileName : NameModification
    , jsonModulesImports : JsonModulesImports
    , unionTypeGeneratorFeatures : List UnionTypeGeneratorFeature
    }


{-| //DefaultValue
-}
initConfig : Config
initConfig =
    { genCommand = Nothing
    , encodersName = { prefix = "", suffix = "Encoder", providedName = DontTouch }
    , decodersName = { prefix = "", suffix = "Decoder", providedName = DontTouch }
    , outputFileName = { prefix = "", suffix = "", providedName = DontTouch }
    , jsonModulesImports =
        { decode = Replace "JD"
        , encode = Replace "JE"
        }
    , unionTypeGeneratorFeatures = []
    }
