module TransformationTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Transformation exposing (..)
import Set


suite : Test
suite =
    let
        context =
            { defaultContext | decoderPrefix = "" }
    in
        describe "Ast transformation"
            [ describe "Multiple fields"
                [ test "Generates decoder for record type with one field" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ) ])))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])))
                , test "Generates decoder for record type with two fields" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "String" ] [] ) ])))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "string" ]))))
                , test "Generates decoder for record type with three fields" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| (TypeRecord ([ ( "a", TypeConstructor [ "Int" ] [] ), ( "b", TypeConstructor [ "String" ] [] ), ( "c", TypeConstructor [ "Float" ] [] ) ])))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "string" ])) (Application (Application (Variable [ "required" ]) (String "c")) (Variable [ "float" ])))))
                ]
            , describe
                "Complex fields"
                [ test "Generates decoder for record type with complex field type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| (TypeRecord ([ ( "a", TypeConstructor [ "List" ] ([ TypeConstructor [ "Int" ] [] ]) ) ])))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Application (Variable [ "list" ]) (Variable [ "int" ]))))
                , test "Generates decoder for record type with complex(nested) field type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| (TypeRecord ([ ( "a", TypeConstructor [ "List" ] ([ TypeTuple ([ TypeConstructor [ "Array" ] ([ TypeConstructor [ "Bool" ] [] ]) ]) ]) ) ])))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (Application (Variable [ "list" ]) (Application (Variable [ "array" ]) (Variable [ "bool" ])))))
                , test "Generates decoder for record type with tuple field" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForRecord context "Basic" <| ((TypeRecord ([ ( "a", TypeTuple ([ TypeConstructor [ "Int" ] [], TypeConstructor [ "Float" ] [], TypeConstructor [ "String" ] [] ]) ) ]))))
                            (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (Application (Application (Variable [ "required" ]) (String "a")) (BinOp (Variable [ "|>" ]) (Application (Variable [ "succeed" ]) (Variable [ ",," ])) (BinOp (Variable [ "|>" ]) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 0)) (Variable [ "int" ]))) (BinOp (Variable [ "|>" ]) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 1)) (Variable [ "float" ]))) (Application (Variable [ "custom" ]) (Application (Application (Variable [ "index" ]) (Integer 2)) (Variable [ "string" ]))))))))
                ]
            , describe "Union types"
                [ test "Generates decoder for simple union type" <|
                    \_ ->
                        Expect.equal
                            (genDecoderForUnionType context <| TypeDeclaration (TypeConstructor [ "T" ] []) ([ TypeConstructor [ "A" ] [], TypeConstructor [ "B" ] ([ TypeConstructor [ "Int" ] [] ]), TypeConstructor [ "C" ] [] ]))
                            (Application (Variable [ "oneOf" ]) (List ([ Application (Application (Variable [ "field" ]) (String "A")) (Application (Variable [ "succeed" ]) (Variable [ "A" ])), Application (Application (Variable [ "field" ]) (String "B")) (Application (Application (Variable [ "map" ]) (Variable [ "B" ])) (Variable [ "int" ])), Application (Application (Variable [ "field" ]) (String "C")) (Application (Variable [ "succeed" ]) (Variable [ "C" ])) ])))
                ]
            ]
