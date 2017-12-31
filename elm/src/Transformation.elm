module Transformation exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)


genDecoder : Statement -> Statement
genDecoder stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart
            in
                FunctionDeclaration (typeName ++ "Decoder") [] <| genDecoderForRecord typeName "" rightPart

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


genDecoderForRecord : String -> String -> Type -> Expression
genDecoderForRecord typeName accessor recordAst =
    let
        decodeApp =
            (Application (Variable [ "decode" ]) (Variable [ typeName ]))
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
        typeDecoder =
            decodeType type_
    in
        Application (Application (Variable [ "required" ]) (String name)) (typeDecoder)


decodeType type_ =
    case type_ of
        TypeConstructor [ typeName ] argsTypes ->
            let
                firstType =
                    variable <| String.toLower typeName
            in
                case argsTypes of
                    [] ->
                        firstType

                    l ->
                        List.foldl (\item accum -> Application accum (decodeType item)) firstType l

        TypeTuple [ a ] ->
            (decodeType a)

        TypeTuple l ->
            case List.reverse <| List.indexedMap (\i x -> ( i, x )) l of
                [] ->
                    Debug.crash "Empty TypeTuple is not allowed!"

                a :: cons ->
                    let
                        pipelineStart =
                            pipeOp <| Application (variable "succeed") (variable <| String.repeat (List.length cons) ",")
                    in
                        pipelineStart <| List.foldl (\item accum -> pipeOp (tupleFieldDec item) accum) (tupleFieldDec a) cons

        _ ->
            Debug.crash "Not allowed type in recordField"


tupleFieldDec ( index, type_ ) =
    Application (variable "custom") (Application (Application (variable "index") (Integer index)) (decodeType type_))


variable x =
    Variable [ x ]


pipeOp =
    BinOp (Variable [ "|>" ])


getTypeName t =
    case t of
        TypeConstructor [ name ] [] ->
            name

        _ ->
            Debug.crash "Cannot extract typeName!"
