module Model exposing (..)

import Set
import Dict
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Config exposing (NameModification, UnionTypeGeneratorFeature(..), ProvidedNameModification(..))


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



-- #Ignore


type alias TypeSet =
    Set.Set String



-- #Ignore


type alias Model =
    { inputFileContent : String
    , outputFileName : String
    , rootDir : String
    , genCommand : GenCommand
    , parsedStatements : List Statement
    , newlyParsedStatements : List Statement
    , unknownTypes : TypeSet
    , typesDict : Dict.Dict String Statement
    , dependencies : ( TypeSet, Dict.Dict String TypeSet )
    , importsDict : Dict.Dict (List String) TypeSet
    , moduleDeclaration : Statement
    , providedDecoders : Dict.Dict String String
    , providedEncoders : Dict.Dict String String
    , defaultRecordValues : Dict.Dict ( String, String ) Expression
    , defaultUnionValues : Dict.Dict String Expression
    , generatedDecoders : List (List Statement)
    , generatedEncoders : List (List Statement)
    , config : Config
    }


initModel =
    { inputFileContent = ""
    , genCommand = Decoders
    , outputFileName = ""
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
    , generatedDecoders = []
    , generatedEncoders = []
    , config = initConfig
    }


type alias Config =
    { genCommand : Maybe GenCommand
    , encodersName : NameModification
    , decodersName : NameModification
    , outputFileName : NameModification
    , unionTypeGeneratorFeatures : List UnionTypeGeneratorFeature
    }


initConfig =
    { genCommand = Nothing
    , encodersName = { prefix = "", suffix = "Encoder", providedName = DontTouch }
    , decodersName = { prefix = "", suffix = "Decoder", providedName = DontTouch }
    , outputFileName = { prefix = "", suffix = "", providedName = DontTouch }
    , unionTypeGeneratorFeatures = []
    }
