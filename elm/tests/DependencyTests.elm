module DependencyTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import Dependency exposing (..)


suite : Test
suite =
    let
        knownTypes =
            Set.fromList [ "Bool", "Int", "Float", "String", "List", "Array" ]
    in
        describe "Dependency:"
            [ describe "can get dependencies from type"
                [ test "dont extract standard types as a dependencies" <|
                    \_ ->
                        Expect.equal
                            (getDependencies knownTypes <| TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Trivial" ] [], TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "String" ] [] ]) ]) ]) ]))
                            (Set.empty)
                , test "can extract dependencies from union type" <|
                    \_ ->
                        Expect.equal
                            (getDependencies knownTypes <| TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Type1" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "Type2" ] [] ]) ]) ]) ]))
                            (Set.fromList [ "Type1", "Type2" ])
                , test "can extract dependencies from record type" <|
                    \_ ->
                        Expect.equal
                            (getDependencies knownTypes <| TypeAliasDeclaration (TypeConstructor [ "Record" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "A" ] ([ TypeConstructor [ "B" ] [], TypeConstructor [ "C" ] [] ]) ), ( "field2", TypeConstructor [ "Basic" ] [] ) ])))
                            (Set.fromList [ "A", "B", "C", "Basic" ])
                ]
            , describe "can build dependency graph"
                [ test "for 2 simple union types" <|
                    \_ ->
                        Expect.equal
                            (makeDependencyGraph knownTypes
                                [ TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Trivial" ] [], TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "String" ] [] ]) ]) ]) ])
                                , TypeAliasDeclaration (TypeConstructor [ "Record" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "List" ] ([ TypeConstructor [ "Float" ] [] ]) ), ( "field2", TypeConstructor [ "Basic" ] [] ) ]))
                                ]
                            )
                            (Dict.fromList [ ( "Record", Set.fromList [ "Basic" ] ) ])
                ]
            ]
