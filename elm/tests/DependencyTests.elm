module DependencyTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import Dependency exposing (..)
import Model exposing (..)
import Composer exposing (..)


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
                            (makeDependencyGraph Set.empty
                                knownTypes
                                [ TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Trivial" ] [], TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "Cons2" ] ([ TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "String" ] [] ]) ]) ]) ])
                                , TypeAliasDeclaration (TypeConstructor [ "Record" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "List" ] ([ TypeConstructor [ "Float" ] [] ]) ), ( "field2", TypeConstructor [ "Basic" ] [] ) ]))
                                ]
                            )
                            ( Set.fromList [ "Record" ], Dict.fromList [ ( "Record", Set.fromList [ "Basic" ] ), ( "Basic", Set.empty ) ] )
                , test "for 3 simple types" <|
                    \_ ->
                        Expect.equal
                            (makeDependencyGraph Set.empty
                                knownTypes
                                [ TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "A" ] [] ])
                                , TypeDeclaration (TypeConstructor [ "B" ] []) ([ TypeConstructor [ "B" ] ([ TypeConstructor [ "A" ] [] ]) ])
                                , TypeAliasDeclaration (TypeConstructor [ "C" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "B" ] [] ), ( "field2", TypeConstructor [ "A" ] [] ) ]))
                                ]
                            )
                            ( Set.fromList [ "C" ], Dict.fromList [ ( "B", Set.fromList [ "A" ] ), ( "C", Set.fromList [ "B", "A" ] ), ( "A", Set.empty ) ] )
                , test "for 3 simple types out of dependency order" <|
                    \_ ->
                        Expect.equal
                            (makeDependencyGraph Set.empty
                                knownTypes
                                [ TypeAliasDeclaration (TypeConstructor [ "C" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "B" ] [] ), ( "field2", TypeConstructor [ "A" ] [] ) ]))
                                , TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "A" ] [] ])
                                , TypeDeclaration (TypeConstructor [ "B" ] []) ([ TypeConstructor [ "B" ] ([ TypeConstructor [ "A" ] [] ]) ])
                                ]
                            )
                            ( Set.fromList [ "C" ], Dict.fromList [ ( "B", Set.fromList [ "A" ] ), ( "C", Set.fromList [ "B", "A" ] ), ( "A", Set.empty ) ] )
                ]
            , describe "can resolve dependencies"
                [ test "in case of load of additional statements" <|
                    \_ ->
                        let
                            initialModel =
                                { initModel
                                    | parsedStatements =
                                        [ TypeDeclaration (TypeConstructor [ "B" ] []) ([ TypeConstructor [ "B" ] ([ TypeConstructor [ "A" ] [] ]) ])
                                        , TypeAliasDeclaration (TypeConstructor [ "C" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "B" ] [] ), ( "field2", TypeConstructor [ "A" ] [] ) ]))
                                        ]
                                }

                            firstPass =
                                resolveDependencies initialModel

                            modelWithDeps =
                                { firstPass | newlyParsedStatements = [ TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "A" ] [] ]) ] }

                            finalForm =
                                resolveDependencies modelWithDeps
                        in
                            Expect.all
                                [ (\m -> Expect.equal m.unknownTypes Set.empty)
                                , (\m -> Expect.equal m.newlyParsedStatements [])
                                , (\m -> Expect.equal (Dict.keys m.typesDict) [ "A", "B", "C" ])
                                , (\m -> Expect.equal (Dict.keys <| Tuple.second m.dependencies) [ "A", "B", "C" ])
                                , (\m -> Expect.equal (Tuple.first m.dependencies) (Set.fromList [ "C" ]))
                                ]
                                finalForm
                ]
            ]
