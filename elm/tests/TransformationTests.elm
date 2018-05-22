module TransformationTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Transformation.Shared exposing (..)
import Transformation.Decoders exposing (..)
import Transformation.Encoders exposing (..)
import Set
import Dict
import ReadConfig exposing (..)
import TypeName


suite : Test
suite =
    let
        context =
            initContext True "" defaultDecoderNameFunc Dict.empty Dict.empty Dict.empty Set.empty

        encContext =
            initContext False "" defaultEncoderNameFunc Dict.empty Dict.empty Dict.empty Set.empty
    in
        describe "Ast transformation"
            [ describe "Multiple fields"
                [ test "Generates decoder for record type with one field" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ) ])))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ]))))
                , test "Generates decoder for record type with two fields" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "String" ] [] ) ])))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "string" ])))))
                , test "Generates decoder for record type with three fields" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "String" ] [] ), ( "c", TypeConstructor [ "Float" ] [] ) ])))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "string" ])) (Application (Application (Variable [ "required" ]) (String "c")) (Variable [ "float" ]))))))
                ]
            , describe
                "Encoders"
                [ test "Generates encoder for record type with one field" <|
                    \_ ->
                        Expect.equal
                            (genEncoderForRecord encContext (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ) ])))
                            (Ok (Application (Variable [ "object" ]) (List ([ Tuple ([ String "a", Application (Variable [ "int" ]) (Access (Variable [ "value" ]) [ "a" ]) ]) ]))))
                , test "Generates encoder for record type with two fields" <|
                    \_ ->
                        Expect.equal
                            (genEncoderForRecord encContext (TypeName.fromStr "Basic") <| ((TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "String" ] [] ) ]))))
                            (Ok (Application (Variable [ "object" ]) (List ([ Tuple ([ String "a", Application (Variable [ "int" ]) (Access (Variable [ "value" ]) [ "a" ]) ]), Tuple ([ String "b", Application (Variable [ "string" ]) (Access (Variable [ "value" ]) [ "b" ]) ]) ]))))
                , test "Generates encoder for simple union type" <|
                    \_ ->
                        Expect.equal
                            (genEncoderForUnionType encContext <| TypeDeclaration (TypeConstructor [ "T" ] []) ([ TypeConstructor [ "A" ] [], TypeConstructor [ "B" ] [], TypeConstructor [ "C" ] [] ]))
                            (Ok (Case (Variable [ "value" ]) ([ ( Variable [ "A" ], Application (Variable [ "object" ]) (List ([ Tuple ([ String "A", Variable [ "null" ] ]) ])) ), ( Variable [ "B" ], Application (Variable [ "object" ]) (List ([ Tuple ([ String "B", Variable [ "null" ] ]) ])) ), ( Variable [ "C" ], Application (Variable [ "object" ]) (List ([ Tuple ([ String "C", Variable [ "null" ] ]) ])) ) ])))
                , test "Generates encoder for non-trivial union type" <|
                    \_ ->
                        Expect.equal
                            (genEncoderForUnionType encContext <| TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "A" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "B" ] ([ TypeConstructor [ "Float" ] [] ]) ]))
                            (Ok (Case (Variable [ "value" ]) ([ ( Application (Variable [ "A" ]) (Variable [ "v1" ]), Application (Variable [ "object" ]) (List ([ Tuple ([ String "A", Application (Variable [ "int" ]) (Variable [ "v1" ]) ]) ])) ), ( Application (Variable [ "B" ]) (Variable [ "v1" ]), Application (Variable [ "object" ]) (List ([ Tuple ([ String "B", Application (Variable [ "float" ]) (Variable [ "v1" ]) ]) ])) ) ])))
                , test "Generates encoder for union type with list" <|
                    \_ ->
                        Expect.equal
                            (genEncoderForUnionType encContext <| TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "A" ] ([ TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "Int" ] [] ]) ]) ]) ]))
                            (Ok (Case (Variable [ "value" ]) ([ ( Application (Variable [ "A" ]) (Variable [ "v1" ]), Application (Variable [ "object" ]) (List ([ Tuple ([ String "A", Application (Application (Variable [ "listEncoder" ]) (Variable [ "int" ])) (Variable [ "v1" ]) ]) ])) ) ])))
                ]
            , describe
                "Complex fields"
                [ test "Generates decoder for record type with complex field type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "List" ] ([ TypeConstructor [ "Int" ] [] ]) ) ])))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Application (Variable [ "list" ]) (Variable [ "int" ])))))
                , test "Generates decoder for record type with complex(nested) field type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| (TypeRecord ([ ( "a", TypeConstructor [ "List" ] ([ TypeTuple ([ TypeConstructor [ "Array" ] ([ TypeConstructor [ "Bool" ] [] ]) ]) ]) ) ])))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Application (Variable [ "list" ]) (Application (Variable [ "array" ]) (Variable [ "bool" ]))))))
                , test "Generates decoder for record type with tuple field" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForTypeAlias context (TypeName.fromStr "Basic") <| ((TypeRecord ([ ( "a", TypeTuple ([ TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [], TypeConstructor [ "String" ] [] ]) ) ]))))
                            (Ok (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (BinOp (Variable [ "|>" ]) (Application (Variable [ "succeed" ]) (Variable [ "(,,)" ])) (BinOp (Variable [ "|>" ]) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 0)) (Variable [ "int" ]))) (BinOp (Variable [ "|>" ]) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 1)) (Variable [ "float" ]))) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 2)) (Variable [ "string" ])))))))))
                ]
            , describe "Union types"
                [ test "Generates decoder for simple union type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForUnionType context <| TypeDeclaration (TypeConstructor [ "T" ] []) ([ TypeConstructor [ "A" ] [], TypeConstructor [ "B" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "C" ] [] ]))
                            (Ok (Application (Variable [ "oneOf" ]) (List ([ Application (Application (Variable [ "field" ]) (String "A")) (Application (Variable [ "succeed" ]) (Variable [ "A" ])), Application (Application (Variable [ "field" ]) (String "B")) (Application (Application (Variable [ "map" ]) (Variable [ "B" ])) (Variable [ "int" ])), Application (Application (Variable [ "field" ]) (String "C")) (Application (Variable [ "succeed" ]) (Variable [ "C" ])) ]))))
                , test "Correctly generates expr for multi-arg constructors" <|
                    \_ ->
                        Expect.equal
                            (decodeUnionTypeArgs context (TypeName.fromStr "Cons2") [ TypeConstructor [ "String" ] [], TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [] ])
                            (Ok (Application (Application (Application (Application (variable "" "map3") (variable "" "Cons2")) (Application (Application (Variable [ "index" ]) (Integer 0)) (variable "" "string"))) (Application (Application (Variable [ "index" ]) (Integer 1)) (variable "" "int"))) (Application (Application (Variable [ "index" ]) (Integer 2)) (variable "" "float"))))
                , test "Correctly generates encoder for multi-arg constructors" <|
                    \_ ->
                        Expect.equal
                            (encodeUnionTypeArgs context "Cons2" [ TypeConstructor [ "String" ] [], TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [] ])
                            (Ok (Tuple [ String "Cons2", Application (Variable [ "list" ]) (List ([ Application (Variable [ "string" ]) (Variable [ "v1" ]), Application (Variable [ "int" ]) (Variable [ "v2" ]), Application (Variable [ "float" ]) (Variable [ "v3" ]) ])) ]))
                ]
            , describe "Encode and mapping"
                [ test "can correctly encode list of ints" <|
                    \_ ->
                        Expect.equal (encodeType encContext <| TypeConstructor [ "List" ] ([ TypeConstructor [ "Int" ] [] ]))
                            (Ok (Application (Variable [ "listEncoder" ]) (Variable [ "int" ])))
                ]
            , test "Generates decoder with default constructor" <|
                \_ ->
                    Expect.equal
                        (genDecoderForUnionType { context | assumeUnionTypeDefaultConstructor = True } <|
                            TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Trivial" ] [], TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]) ])
                        )
                        (Ok (Application (Variable [ "oneOf" ]) (List ([ Application (Application (Variable [ "field" ]) (String "Trivial")) (Application (Variable [ "succeed" ]) (Variable [ "Trivial" ])), Application (Application (Variable [ "field" ]) (String "Cons1")) (Application (Application (Variable [ "map" ]) (Variable [ "Cons1" ])) (Variable [ "int" ])), Application (Variable [ "succeed" ]) (Variable [ "Trivial" ]) ]))))
            , test "dont generate decoder with default constructor if its non-trivial" <|
                \_ ->
                    let
                        typeDecl =
                            TypeDeclaration (TypeConstructor [ "Basic" ] []) ([ TypeConstructor [ "Trivial" ] ([ TypeConstructor [ "Float" ] [] ]), TypeConstructor [ "Cons1" ] ([ TypeConstructor [ "Int" ] [] ]) ])
                    in
                        Expect.equal
                            (genDecoderForUnionType { context | assumeUnionTypeDefaultConstructor = True } <| typeDecl)
                            (genDecoderForUnionType context typeDecl)
            ]
