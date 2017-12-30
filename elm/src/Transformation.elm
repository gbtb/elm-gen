module Transformation exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)


genDecoderForRecord : String -> String -> Type -> Expression
genDecoderForRecord typeName accessor recordAst =
    let
        decodeApp =
            (Application (Variable [ "decode" ]) (Variable [ typeName ]))

        pipeOp =
            BinOp (Variable [ "|>" ])
    in
        case recordAst of
            TypeRecord l ->
                case List.reverse l of
                    a :: cons ->
                        pipeOp decodeApp <|
                            List.foldl (\item accum -> pipeOp (recordFieldDec item) accum) (recordFieldDec a) cons

                    _ ->
                        Debug.crash "Too much fields"

            _ ->
                Debug.crash "It is not a record!"


recordFieldDec ( name, type_ ) =
    let
        typeName =
            case type_ of
                TypeConstructor [ typeName ] _ ->
                    String.toLower typeName

                _ ->
                    Debug.crash "Not allowed type in recordField"
    in
        Application (Application (Variable [ "required" ]) (String name)) (Variable [ typeName ])
