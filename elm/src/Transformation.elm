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
            TypeRecord [a] ->
                pipeOp decodeApp <|
                    recordFieldDec a

            _ ->
                Debug.crash "It is not a record!"


recordFieldDec ( name, type_ ) =
    let typeName = 
        case type_ of
            TypeConstructor [typeName] _-> String.toLower typeName
            _-> Debug.crash "Not allowed type in recordField"
    in
    Application (Application (Variable [ "required" ]) (String name)) (Variable [ typeName ])