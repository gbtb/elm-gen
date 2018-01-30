module Model exposing (..)

import Set
import Dict
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)


type GenCommand
    = Decoders
    | Encoders
    | DecodersAndEncoders



-- #Ignore


type alias TypeSet =
    Set.Set String



-- #Ignore


type alias Model =
    { inputFileContent : String
    , parsedStatements : List Statement
    , newlyParsedStatements : List Statement
    , unknownTypes : TypeSet
    , typesDict : Dict.Dict String Statement
    , dependencies : ( TypeSet, Dict.Dict String TypeSet )
    , importsDict : Dict.Dict (List String) (Set.Set ( String, Int ))
    , moduleDeclaration : Statement
    , generatedDecoders : List (List Statement)
    }


initModel =
    { inputFileContent = ""
    , parsedStatements = []
    , newlyParsedStatements = []
    , unknownTypes = Set.empty
    , typesDict = Dict.empty
    , dependencies = ( Set.empty, Dict.empty )
    , importsDict = Dict.empty
    , moduleDeclaration = Comment "Init placeholder"
    , generatedDecoders = []
    }
