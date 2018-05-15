module Transformation.Decoders exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Set
import Dict
import Char
import Utils exposing (..)
import List.Extra as List
import ReadConfig exposing (..)
import Model exposing (TypeName)
import TypeName
import Transformation.Shared exposing (..)


genDecoder : TransformationContext -> Statement -> List Statement
genDecoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart
            in
                genDecoderHelper context leftPart rightPart (genDecoderForTypeAlias context typeName rightPart)

        TypeDeclaration leftPart rightPart ->
            genDecoderHelper context leftPart rightPart (genDecoderForUnionType context stmt)

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


genDecoderHelper context leftPart rightPart generatorInvocation =
    let
        typeName =
            getTypeName leftPart

        decoderName =
            TypeName.getDecoderName typeName context.makeName

        decoderType =
            if String.length context.decoderPrefix > 0 then
                [ context.decoderPrefix, "Decoder" ]
            else
                [ "Decoder" ]

        decoderDeclaration =
            FunctionTypeDeclaration decoderName <| TypeConstructor decoderType ([ leftPart ])

        decoderBody =
            FunctionDeclaration (decoderName) [] <| generatorInvocation
    in
        if Set.member typeName context.dontDeclareTypes then
            List.singleton decoderBody
        else
            [ decoderDeclaration, decoderBody ]


genDecoderForUnionType : TransformationContext -> Statement -> Expression
genDecoderForUnionType ctx unionType =
    let
        begin =
            Application (variable ctx.decoderPrefix "oneOf")
    in
        case unionType of
            TypeDeclaration (TypeConstructor typeName []) constructors ->
                begin <|
                    List <|
                        addDefaultConstructorDecoder ctx typeName constructors <|
                            List.map (genDecoderForUnionTypeConstructor ctx) constructors

            _ ->
                Debug.crash "It is not a union type!"


addDefaultConstructorDecoder ctx typeName constructors generatedCode =
    case Dict.get typeName ctx.defaultUnionValues of
        Just defaultValue ->
            generatedCode ++ [ Application (variable ctx.decoderPrefix "succeed") defaultValue ]

        Nothing ->
            if not ctx.assumeUnionTypeDefaultConstructor then
                generatedCode
            else
                List.head constructors
                    |> Maybe.andThen
                        (\h ->
                            case h of
                                TypeConstructor [ name ] [] ->
                                    Just (Application (variable ctx.decoderPrefix "succeed") (variable "" name))

                                _ ->
                                    Nothing
                        )
                    |> Maybe.map (\h -> generatedCode ++ [ h ])
                    |> Maybe.withDefault generatedCode


genDecoderForUnionTypeConstructor ctx cons =
    case cons of
        TypeConstructor typename args ->
            Application
                (Application (variable ctx.decoderPrefix "field")
                    (String <| TypeName.toStr typename)
                )
                (decodeUnionTypeArgs ctx typename args)

        _ ->
            Debug.crash "Invalid union type constructor!"


decodeUnionTypeArgs ctx name args =
    let
        n =
            List.length args

        start =
            Application
                (variable ctx.decoderPrefix <| getMapFun n)
                (Variable name)

        indexAppl idx =
            Application (Application (variable ctx.decoderPrefix "index") (Integer idx))
    in
        case args of
            [] ->
                (Application (variable ctx.decoderPrefix "succeed") (Variable name))

            [ a ] ->
                Application start (decodeType ctx a)

            l ->
                List.indexedFoldl (\idx item accum -> Application accum (indexAppl idx <| decodeType ctx item)) start l


genDecoderForTypeAlias : TransformationContext -> TypeName -> Type -> Expression
genDecoderForTypeAlias ctx typeName recordAst =
    let
        decodeApp =
            (Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable typeName))
    in
        case recordAst of
            TypeRecord l ->
                case List.reverse l of
                    a :: cons ->
                        pipeOp decodeApp <|
                            List.foldl (\item accum -> pipeOp (recordFieldDec ctx typeName item) accum) (recordFieldDec ctx typeName a) cons

                    _ ->
                        Debug.crash "Too much fields"

            TypeConstructor _ _ ->
                decodeType ctx recordAst

            --genDecoderForUnionTypeConstructor ctx recordAst
            _ ->
                Debug.crash "It is not a record!"


recordFieldDec ctx typeName ( name, type_ ) =
    let
        typeDecoder ctx =
            decodeType ctx type_

        appTemplate funcName =
            Application (Application (Variable <| qualifiedName ctx.decoderPrefix funcName) (String name)) (typeDecoder ctx)
    in
        case Dict.get ( typeName, name ) ctx.defaultRecordValues of
            Just defaultValue ->
                Application (appTemplate "optional") defaultValue

            Nothing ->
                appTemplate "required"


decodeType ctx type_ =
    case type_ of
        TypeConstructor typeName argsTypes ->
            decodeKnownTypeConstructor ctx typeName argsTypes

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
                                    (variable "" <| String.repeat (List.length cons) ",")
                    in
                        pipelineStart <| List.foldl (\item accum -> pipeOp (tupleFieldDec ctx item) accum) (tupleFieldDec ctx a) cons

        _ ->
            Debug.crash "Not allowed type in recordField"


genMaybeDecoder nameFunc =
    FunctionDeclaration (nameFunc "maybe")
        ([ Variable [ "decoder" ] ])
        (Application
            (Access (Variable [ "JD" ]) [ "oneOf" ])
            (List
                ([ Application (Access (Variable [ "JD" ]) [ "null" ]) (Variable [ "Nothing" ])
                 , Application (Application (Access (Variable [ "JD" ]) [ "map" ]) (Variable [ "Just" ])) (Variable [ "decoder" ])
                 ]
                )
            )
        )


decodeKnownTypeConstructor ctx typeName argsTypes =
    let
        firstType =
            Dict.get typeName ctx.knownTypes
                |> fromJust ("Unknown type somehow leaked to transformation stage." ++ TypeName.toStr typeName)
                |> Variable
    in
        case argsTypes of
            [] ->
                firstType

            l ->
                List.foldl (\item accum -> Application accum (decodeType ctx item)) firstType l


tupleFieldDec ctx ( index, type_ ) =
    Application (variable ctx.decoderPrefix "custom")
        (Application (Application (variable ctx.decoderPrefix "index") (Integer index)) (decodeType ctx type_))


pipeOp =
    BinOp (Variable [ "|>" ])


getMapFun n =
    case n of
        1 ->
            "map"

        x ->
            "map" ++ (toString x)
