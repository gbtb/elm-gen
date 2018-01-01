module PrinterTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Printer exposing (..)


printExpression =
    Printer.printExpression initContext


suite : Test
suite =
    describe "Printer can print"
        [ describe "expressions"
            [ test "Single application" <|
                \_ ->
                    Expect.equal
                        (printExpression (Application (Variable [ "index" ]) (Integer 0)))
                        (Line 0 "index 0")
            , test "Multi-arg application" <|
                \_ ->
                    Expect.equal
                        (printExpression (Application (Application (Variable [ "index" ]) (Integer 0)) (Variable [ "int" ])))
                        (Line 0 "index 0 int")
            , test "prints pipe bin op" <|
                \_ ->
                    Expect.equal
                        (printExpression (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "int" ])))))
                        (Lines
                            [ Line 0 "decode Basic"
                            , Line 1
                                "|> required \"a\" int"
                            , Line
                                1
                                "|> required \"b\" int"
                            ]
                        )
            ]
        , describe "statements"
            [ test "Simple function statement" <|
                \_ ->
                    Expect.equal
                        (printStatement (FunctionDeclaration "sum" ([ Variable [ "a" ], Variable [ "b" ] ]) (BinOp (Variable [ "+" ]) (Variable [ "a" ]) (Variable [ "b" ]))))
                        (Lines
                            [ Line 0 "sum a b ="
                            , Line 1 "a + b"
                            ]
                        )
            , test "Simple decoder" <|
                \_ ->
                    Expect.equal
                        (printStatement (FunctionDeclaration "basicDecoder" [] (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "int" ]))))))
                        (Lines
                            [ Line 0 "basicDecoder ="
                            , Line 1 "decode Basic"
                            , Line 2 "|> required \"a\" int"
                            , Line 2 "|> required \"b\" int"
                            ]
                        )
            , test "type1" <|
                \_ ->
                    Expect.equal
                        (printType <| TypeConstructor [ "JD", "Decoder" ] ([ TypeConstructor [ "String" ] [] ]))
                        ("JD.Decoder String")
            , test "type2" <|
                \_ ->
                    Expect.equal
                        (printType <| TypeConstructor [ "Dict" ] ([ TypeConstructor [ "Int" ] [], TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "Char" ] [] ]) ]) ]))
                        ("Dict Int (List Char)")
            ]
        ]
