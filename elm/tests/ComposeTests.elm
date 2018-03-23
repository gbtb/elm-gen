module ComposeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Composer exposing (..)
import StatementFilters exposing (..)
import Model exposing (..)
import Dict
import Set
import PrintRepr exposing (..)


suite : Test
suite =
    describe "Composer:"
        [ test "can store types in a dict" <|
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
        , test "can form request for files to load types declarations" <|
            \_ ->
                Expect.equal
                    (makeFileLoadRequest
                        ({ initModel
                            | parsedStatements =
                                [ ImportStatement [ "Module1" ] Nothing (Just (SubsetExport ([ TypeExport "Type1" Nothing ])))
                                , ImportStatement [ "Module2" ] Nothing (Just (SubsetExport ([ TypeExport "Type2" (Just AllExport), TypeExport "Type3" (Just (SubsetExport ([ FunctionExport "A" ]))) ])))
                                , ImportStatement [ "Rel", "Module3" ] Nothing (Just (SubsetExport ([ TypeExport "Type4" Nothing ])))
                                , ImportStatement [ "Module4" ] Nothing (Just (AllExport))
                                ]
                            , unknownTypes = Set.fromList [ "Type1", "Type2", "Type3", "Type4" ]
                         }
                        )
                    )
                    (Ok <|
                        Dict.fromList
                            [ ( [ "Module1" ], Set.fromList [ "Type1" ] )
                            , ( [ "Module2" ], Set.fromList [ "Type2", "Type3" ] )
                            , ( [ "Rel", "Module3" ], Set.fromList [ "Type4" ] )
                            ]
                    )
        , test "can extract encoders" <|
            \_ ->
                Expect.equal
                    (extractEncoder [ "Value" ] <| FunctionTypeDeclaration "listEncoder" (TypeApplication (TypeApplication (TypeVariable "a") (TypeConstructor [ "Value" ] [])) (TypeApplication (TypeConstructor [ "List" ] ([ TypeVariable "a" ])) (TypeConstructor [ "Value" ] []))))
                    (Just ( "List", "listEncoder" ))
        , test "can extract encoders 2" <|
            \_ ->
                Expect.equal
                    (extractEncoder [ "Value" ] <| FunctionTypeDeclaration "basicEncoder" (TypeApplication (TypeConstructor [ "Basic" ] []) (TypeConstructor [ "Value" ] [])))
                    (Just ( "Basic", "basicEncoder" ))
        , test "prints correct default packages import for decoders only" <|
            \_ ->
                Expect.equal
                    (printImports Decoders Dict.empty Dict.empty)
                    ([ Line 0 "import Json.Decode as JD", Line 0 "import Json.Decode.Pipeline as JD" ])
        , test "prints correct default packages import for decoders and encoders" <|
            \_ ->
                Expect.equal
                    (printImports DecodersAndEncoders Dict.empty Dict.empty)
                    ([ Line 0 "import Json.Decode as JD", Line 0 "import Json.Decode.Pipeline as JD", Line 0 "import Json.Encode as JE" ])
        , test "prints correct default packages import for encoders only" <|
            \_ ->
                Expect.equal
                    (printImports Encoders Dict.empty Dict.empty)
                    ([ Line 0 "import Json.Encode as JE" ])
        ]
