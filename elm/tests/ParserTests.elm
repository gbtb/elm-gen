module ParserTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import ParserExtensions exposing (..)
import StatementFilters exposing (..)


suite : Test
suite =
    describe "ParserExtensions:"
        [ test "deletes types with ignore metacomment from list of statements" <|
            \_ ->
                Expect.equal
                    (applyMetaComments
                        [ ModuleDeclaration [ "MetaComments" ] AllExport
                        , Comment " //Ignore"
                        , TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "Trivial" ] [], TypeConstructor [ "B" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "C" ] ([ TypeConstructor [ "String" ] [] ]) ])
                        , FunctionDeclaration "a" [] (BinOp (Variable [ "*" ]) (Variable [ "a" ]) (Integer 3))
                        , Comment " //Ignore"
                        , TypeAliasDeclaration (TypeConstructor [ "R" ] []) (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "List" ] ([ TypeConstructor [ "Char" ] [] ]) ) ]))
                        , TypeDeclaration (TypeConstructor [ "C" ] []) ([ TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeConstructor [ "String" ] [], TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [] ]) ])
                        ]
                    )
                    { statements =
                        [ ModuleDeclaration [ "MetaComments" ] AllExport
                        , FunctionDeclaration "a" [] (BinOp (Variable [ "*" ]) (Variable [ "a" ]) (Integer 3))
                        , TypeDeclaration (TypeConstructor [ "C" ] []) ([ TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeConstructor [ "String" ] [], TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [] ]) ])
                        ]
                    , defaultRecordValues = Dict.empty
                    , defaultUnionValues = Dict.empty
                    }
        , test
            "extracts default union type values"
          <|
            \_ ->
                Expect.equal (extractUnionTypeDefault <| FunctionTypeDeclaration "defaultA" (TypeConstructor [ "A", "B" ] []))
                    (Just "A.B")
        ]
