module Model exposing (..)

import Set
import Dict
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)


type alias Model =
    { inputFileContent : String
    , parsedStatements : List Statement
    , unknownTypes : Set.Set String
    , typesDict : Dict.Dict String Statement
    , dependencies : ( Set.Set String, Dict.Dict String (Set.Set String) )
    , moduleDeclaration : Statement
    , generatedDecoders : List (List Statement)
    }


initModel =
    { inputFileContent = ""
    , parsedStatements = []
    , unknownTypes = Set.empty
    , typesDict = Dict.empty
    , dependencies = ( Set.empty, Dict.empty )
    , moduleDeclaration = Comment "Init placeholder"
    , generatedDecoders = []
    }
