module Model exposing (..)

import Set
import Dict
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)


type alias Model =
    { inputFileContent : String
    , parsedStatements : List Statement
    , moduleDeclaration : Statement
    , generatedDecoders : List (List Statement)
    }


initModel =
    { inputFileContent = ""
    , parsedStatements = []
    , moduleDeclaration = Comment "Init placeholder"
    , generatedDecoders = []
    }
