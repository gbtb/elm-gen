module ComposeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Composer exposing (..)
import Dict


suite : Test
suite =
    describe "Composer:"
        [ test "can make correct module declaration for decoder" <|
            \_ ->
                Expect.equal
                    (makeDecodersModuleDecl <| ModuleDeclaration [ "Basic" ] AllExport)
                    (ModuleDeclaration [ "BasicDecoders" ] AllExport)
        , test "can store types in a dict" <|
            \_ ->
                Expect.equal
                    (makeTypesDict
                        [ TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "A" ] [] ])
                        , TypeDeclaration (TypeConstructor [ "B" ] []) ([ TypeConstructor [ "B" ] ([ TypeConstructor [ "A" ] [] ]) ])
                        , TypeAliasDeclaration (TypeConstructor [ "C" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "B" ] [] ), ( "field2", TypeConstructor [ "A" ] [] ) ]))
                        ]
                    )
                    (Dict.fromList
                        [ ( "A", TypeDeclaration (TypeConstructor [ "A" ] []) ([ TypeConstructor [ "A" ] [] ]) )
                        , ( "B", TypeDeclaration (TypeConstructor [ "B" ] []) ([ TypeConstructor [ "B" ] ([ TypeConstructor [ "A" ] [] ]) ]) )
                        , ( "C", TypeAliasDeclaration (TypeConstructor [ "C" ] []) (TypeRecord ([ ( "field1", TypeConstructor [ "B" ] [] ), ( "field2", TypeConstructor [ "A" ] [] ) ])) )
                        ]
                    )
        ]
