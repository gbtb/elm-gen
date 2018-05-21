module PrinterTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Printer exposing (..)
import PrintRepr exposing (..)


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
                        (Ok (Line 0 "index 0"))
            , test "Multi-arg application" <|
                \_ ->
                    Expect.equal
                        (printExpression (Application (Application (Variable [ "index" ]) (Integer 0)) (Variable [ "int" ])))
                        (Ok (Line 0 "index 0 int"))
            , test "Nested application" <|
                \_ ->
                    Expect.equal
                        (printExpression (Application (Application (Variable [ "required" ]) (String "c")) (Application (Access (Variable [ "JD" ]) [ "list" ]) (Access (Variable [ "JD" ]) [ "string" ]))))
                        (Ok (Line 0 "required \"c\" (JD.list JD.string)"))
            , test "prints pipe bin op" <|
                \_ ->
                    Expect.equal
                        (printExpression (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "int" ])))))
                        (Ok
                            (Lines
                                [ Line 0 "decode Basic"
                                , Line 1
                                    "|> required \"a\" int"
                                , Line
                                    1
                                    "|> required \"b\" int"
                                ]
                            )
                        )
            , test "prints right assoc pipe bin op" <|
                \_ ->
                    Expect.equal
                        (printExpression (BinOp (Variable [ "<|" ]) (Access (Variable [ "JE" ]) [ "list" ]) (Application (Access (Variable [ "List" ]) [ "map" ]) (Variable [ "encoder" ]))))
                        (Ok
                            (Lines
                                [ Line 0 "JE.list <|"
                                , Line 1 "List.map encoder"
                                ]
                            )
                        )
            , test "prints flat list" <|
                \_ ->
                    Expect.equal
                        (Printer.printExpression { initContext | flatList = True } <| Application (Variable [ "list" ]) (List ([ Integer 1, Integer 2, Integer 3 ])))
                        (Ok (Line 0 "list [ 1, 2, 3 ]"))
            , test "Access" <|
                \_ ->
                    Expect.equal
                        (printExpression (Access (Variable [ "JD" ]) [ "null" ]))
                        (Ok (Line 0 "JD.null"))
            , test "Variable" <|
                \_ ->
                    Expect.equal
                        (printExpression (Variable [ "JD", "index" ]))
                        (Ok (Line 0 "JD.index"))
            , test "Tuple" <|
                \_ ->
                    Expect.equal
                        (printExpression <| Tuple ([ String "a", Application (Variable [ "JE", "int" ]) (Access (Variable [ "value" ]) [ "a" ]) ]))
                        (Ok (Line 0 "( \"a\", JE.int value.a )"))
            ]
        , describe "statements"
            [ test "Simple function statement" <|
                \_ ->
                    Expect.equal
                        (printStatement (FunctionDeclaration "sum" ([ Variable [ "a" ], Variable [ "b" ] ]) (BinOp (Variable [ "+" ]) (Variable [ "a" ]) (Variable [ "b" ]))))
                        (Ok
                            (Lines
                                [ Line 0 "sum a b ="
                                , Line 1 "a + b"
                                ]
                            )
                        )
            , test "prints function type decl with nested func args" <|
                \_ ->
                    Expect.equal
                        (Printer.printStatement <| FunctionTypeDeclaration "listEncoder" (TypeApplication (TypeApplication (TypeVariable "a") (TypeConstructor [ "JE", "Value" ] [])) (TypeApplication (TypeConstructor [ "List" ] ([ TypeVariable "a" ])) (TypeConstructor [ "JE", "Value" ] []))))
                        (Ok (Line 0 "listEncoder : (a -> JE.Value) -> List a -> JE.Value"))
            , test "Simple decoder" <|
                \_ ->
                    Expect.equal
                        (printStatement (FunctionDeclaration "basicDecoder" [] (BinOp (Variable [ "|>" ]) (Application (Variable [ "decode" ]) (Variable [ "Basic" ])) (BinOp (Variable [ "|>" ]) (Application (Application (Variable [ "required" ]) (String "a")) (Variable [ "int" ])) (Application (Application (Variable [ "required" ]) (String "b")) (Variable [ "int" ]))))))
                        (Ok
                            (Lines
                                [ Line 0 "basicDecoder ="
                                , Line 1 "decode Basic"
                                , Line 2 "|> required \"a\" int"
                                , Line 2 "|> required \"b\" int"
                                ]
                            )
                        )
            , test "type1" <|
                \_ ->
                    Expect.equal
                        (printType initContext <| TypeConstructor [ "JD", "Decoder" ] ([ TypeConstructor [ "String" ] [] ]))
                        (Ok (Line 0 "JD.Decoder String"))
            , test "type2" <|
                \_ ->
                    Expect.equal
                        (printType initContext <| TypeConstructor [ "Dict" ] ([ TypeConstructor [ "Int" ] [], TypeTuple ([ TypeConstructor [ "List" ] ([ TypeConstructor [ "Char" ] [] ]) ]) ]))
                        (Ok (Line 0 "Dict Int (List Char)"))
            ]
        ]
