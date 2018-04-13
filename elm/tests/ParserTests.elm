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
                    , dontDeclareTypes = Set.empty
                    }
        , test
            "extracts default union type values"
          <|
            \_ ->
                Expect.equal (extractUnionTypeDefault <| FunctionTypeDeclaration "defaultA" (TypeConstructor [ "A", "B" ] []))
                    (Just [ "A", "B" ])
        , test "extracts default record type value" <|
            \_ ->
                Expect.equal (extractRecordTypeDefault <| FunctionTypeDeclaration "initR" (TypeApplication (TypeConstructor [ "R" ] []) (TypeConstructor [ "R" ] [])))
                    (Just [ "R" ])
        , test "extracts default value for union type" <|
            \_ ->
                Expect.equal
                    (extractDefaultValues { initFoldHelper | typeName = Just "A" } <|
                        FunctionDeclaration "defaultA" [] (Application (Variable [ "B" ]) (Integer 3))
                    )
                    (Just <|
                        { initFoldHelper
                            | defaultUnionValues = Dict.fromList [ ( "A", (Application (Variable [ "B" ]) (Integer 3)) ) ]
                        }
                    )
        , test "extracts default value from record type update func" <|
            \_ ->
                Expect.equal
                    (extractDefaultValues { initFoldHelper | typeName = Just "R" } <|
                        FunctionDeclaration "initR" ([ Variable [ "r" ] ]) (RecordUpdate "r" ([ ( "a", Integer 1 ), ( "b", List ([ Integer 1, Integer 2 ]) ), ( "c", String "123" ) ]))
                    )
                    (Just <|
                        { initFoldHelper
                            | defaultRecordValues =
                                Dict.fromList
                                    [ ( ( "R", "a" ), Integer 1 )
                                    , ( ( "R", "b" ), List ([ Integer 1, Integer 2 ]) )
                                    , ( ( "R", "c" ), String "123" )
                                    ]
                        }
                    )
        , test "extracts default value from record type init func" <|
            \_ ->
                Expect.equal
                    (extractDefaultValues { initFoldHelper | typeName = Just "R" } <|
                        FunctionDeclaration "initR" [] (Record ([ ( "a", Integer 1 ), ( "b", List ([ Integer 1, Integer 2 ]) ) ]))
                    )
                    (Just <|
                        { initFoldHelper
                            | defaultRecordValues =
                                Dict.fromList
                                    [ ( ( "R", "a" ), Integer 1 )
                                    , ( ( "R", "b" ), List ([ Integer 1, Integer 2 ]) )
                                    ]
                        }
                    )
        ]
