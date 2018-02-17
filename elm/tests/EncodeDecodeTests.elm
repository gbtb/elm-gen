module EncodeDecodeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Transformation exposing (..)
import Set
import Dict
import Json.Decode as JD


--import Json.Decode.Pipeline as JD


type C
    = Cons1 Int
    | Cons2 String Int Float


suite : Test
suite =
    let
        cDecoder =
            JD.oneOf
                [ JD.field "Cons1" (JD.map Cons1 JD.int)
                , JD.field "Cons2" (JD.map3 Cons2 (JD.index 0 JD.string) (JD.index 1 JD.int) (JD.index 2 JD.float))
                ]
    in
        describe "Using of generated decoders"
            [ test "Can actually decode using generated multi-arg union type decoder" <|
                \_ ->
                    Expect.equal (JD.decodeString cDecoder "{\"Cons2\": [\"hello\", 1, 2.5]}")
                        (Ok (Cons2 "hello" 1 2.5))
            ]