module Transformation exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)


type alias TransformationContext =
    { decoderPrefix : String
    }


genDecoder : TransformationContext -> Statement -> List Statement
genDecoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    String.toLower typeName ++ "Decoder"

                decoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Decoder" ]
                    else
                        [ "Decoder" ]
            in
                [ FunctionTypeDeclaration decoderName <| TypeConstructor decoderType ([ leftPart ])
                , FunctionDeclaration (decoderName) [] <| genDecoderForRecord context typeName rightPart
                ]

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


qualifiedName prefix name =
    if String.length prefix > 0 then
        [ prefix, name ]
    else
        [ name ]


genDecoderForRecord : TransformationContext -> String -> Type -> Expression
genDecoderForRecord ctx typeName recordAst =
    let
        decodeApp =
            (Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable [ typeName ]))
    in
        case recordAst of
            TypeRecord l ->
                case List.reverse l of
                    a :: cons ->
                        pipeOp decodeApp <|
                            List.foldl (\item accum -> pipeOp (recordFieldDec ctx item) accum) (recordFieldDec ctx a) cons

                    _ ->
                        Debug.crash "Too much fields"

            _ ->
                Debug.crash "It is not a record!"


recordFieldDec ctx ( name, type_ ) =
    let
        typeDecoder ctx =
            decodeType ctx type_
    in
        Application (Application (Variable <| qualifiedName ctx.decoderPrefix "required") (String name)) (typeDecoder ctx)


decodeType ctx type_ =
    case type_ of
        TypeConstructor [ typeName ] argsTypes ->
            let
                firstType =
                    variable ctx.decoderPrefix <| String.toLower typeName
            in
                case argsTypes of
                    [] ->
                        firstType

                    l ->
                        List.foldl (\item accum -> Application accum (decodeType ctx item)) firstType l

        TypeTuple [ a ] ->
            (decodeType ctx a)

        TypeTuple l ->
            case List.reverse <| List.indexedMap (\i x -> ( i, x )) l of
                [] ->
                    Debug.crash "Empty TypeTuple is not allowed!"

                a :: cons ->
                    let
                        pipelineStart =
                            pipeOp <|
                                Application
                                    (variable ctx.decoderPrefix "succeed")
                                    (variable ctx.decoderPrefix <| String.repeat (List.length cons) ",")
                    in
                        pipelineStart <| List.foldl (\item accum -> pipeOp (tupleFieldDec ctx item) accum) (tupleFieldDec ctx a) cons

        _ ->
            Debug.crash "Not allowed type in recordField"


tupleFieldDec ctx ( index, type_ ) =
    Application (variable ctx.decoderPrefix "custom")
        (Application (Application (variable ctx.decoderPrefix "index") (Integer index)) (decodeType ctx type_))


variable prefix x =
    Variable <| qualifiedName prefix x


pipeOp =
    BinOp (Variable [ "|>" ])


getTypeName t =
    case t of
        TypeConstructor [ name ] [] ->
            name

        _ ->
            Debug.crash "Cannot extract typeName!"
