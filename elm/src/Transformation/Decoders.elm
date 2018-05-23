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
import Config exposing (getDecodePrefix)
import Result.Extra as Result


genDecoder : TransformationContext -> Statement -> Result String (List Statement)
genDecoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeNameRes =
                    getTypeName leftPart
            in
                Result.andThen
                    (\typeName ->
                        Result.andThen (genDecoderHelper context leftPart rightPart) (genDecoderForTypeAlias context typeName rightPart)
                    )
                    typeNameRes

        TypeDeclaration leftPart rightPart ->
            Result.andThen (genDecoderHelper context leftPart rightPart) (genDecoderForUnionType context stmt)

        _ ->
            Err "Cannot generate decoder for this kind of statement(yet?)"


genDecoderHelper context leftPart rightPart generatorInvocation =
    let
        typeNameRes =
            getTypeName leftPart
    in
        case typeNameRes of
            Ok typeName ->
                let
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
                    Ok <|
                        if Set.member typeName context.dontDeclareTypes then
                            List.singleton decoderBody
                        else
                            [ decoderDeclaration, decoderBody ]

            Err e ->
                Err e


genDecoderForUnionType : TransformationContext -> Statement -> Result String Expression
genDecoderForUnionType ctx unionType =
    let
        begin =
            Application (variable ctx.decoderPrefix "oneOf")
    in
        case unionType of
            TypeDeclaration (TypeConstructor typeName []) constructors ->
                Result.map
                    (\generatedRes ->
                        begin <|
                            List <|
                                addDefaultConstructorDecoder ctx typeName constructors <|
                                    generatedRes
                    )
                    (Result.combine <|
                        List.map (genDecoderForUnionTypeConstructor ctx) constructors
                    )

            _ ->
                Err "It is not a union type!"


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
            Result.map
                (Application
                    (Application (variable ctx.decoderPrefix "field")
                        (String <| TypeName.toStr typename)
                    )
                )
                (decodeUnionTypeArgs ctx typename args)

        _ ->
            Err "Invalid union type constructor!"


decodeUnionTypeArgs ctx name args =
    let
        n =
            List.length args

        start =
            Ok <|
                Application
                    (variable ctx.decoderPrefix <| getMapFun n)
                    (Variable name)

        indexAppl idx =
            Application (Application (variable ctx.decoderPrefix "index") (Integer idx))
    in
        case args of
            [] ->
                Ok (Application (variable ctx.decoderPrefix "succeed") (Variable name))

            [ a ] ->
                Result.map2 Application start (decodeType ctx a)

            l ->
                List.indexedFoldl (\idx item accum -> Result.map2 Application accum (Result.map (indexAppl idx) <| decodeType ctx item)) start l


genDecoderForTypeAlias : TransformationContext -> TypeName -> Type -> Result String Expression
genDecoderForTypeAlias ctx typeName recordAst =
    let
        decodeApp =
            (Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable typeName))
    in
        case recordAst of
            TypeRecord l ->
                case List.reverse l of
                    a :: cons ->
                        Result.map (pipeOp decodeApp) <|
                            List.foldl (\item accum -> Result.map2 pipeOp (recordFieldDec ctx typeName item) accum) (recordFieldDec ctx typeName a) cons

                    _ ->
                        Err "Too much fields"

            TypeConstructor _ _ ->
                decodeType ctx recordAst

            _ ->
                Err "It is not a record!"


recordFieldDec ctx typeName ( name, type_ ) =
    let
        typeDecoder ctx =
            decodeType ctx type_

        appTemplate funcName =
            Result.map (Application (Application (Variable <| qualifiedName ctx.decoderPrefix funcName) (String name))) (typeDecoder ctx)
    in
        case Dict.get ( typeName, name ) ctx.defaultRecordValues of
            Just defaultValue ->
                Result.map (\x -> Application x defaultValue) (appTemplate "optional")

            Nothing ->
                appTemplate "required"


decodeType : { a | knownTypes : Dict.Dict (List String) (List String), decoderPrefix : String } -> Type -> Result String Expression
decodeType ctx type_ =
    case type_ of
        TypeConstructor typeName argsTypes ->
            decodeKnownTypeConstructor ctx typeName argsTypes

        TypeTuple [ a ] ->
            decodeType ctx a

        TypeTuple l ->
            case List.reverse <| List.indexedMap (\i x -> ( i, x )) l of
                [] ->
                    Err "Empty TypeTuple is not allowed!"

                a :: cons ->
                    let
                        pipelineStart =
                            pipeOp <|
                                Application
                                    (variable ctx.decoderPrefix "succeed")
                                    (variable "" <| "(" ++ String.repeat (List.length cons) "," ++ ")")
                    in
                        Result.map pipelineStart <|
                            List.foldl (\item accum -> Result.map2 pipeOp (tupleFieldDec ctx item) accum) (tupleFieldDec ctx a) cons

        _ ->
            Err "Not allowed type in recordField"


genMaybeDecoder conf nameFunc =
    FunctionDeclaration (nameFunc "maybe")
        ([ Variable [ "decoder" ] ])
        (Application
            (Access (Variable [ getDecodePrefix conf ]) [ "oneOf" ])
            (List
                ([ Application (Access (Variable [ getDecodePrefix conf ]) [ "null" ]) (Variable [ "Nothing" ])
                 , Application (Application (Access (Variable [ getDecodePrefix conf ]) [ "map" ]) (Variable [ "Just" ])) (Variable [ "decoder" ])
                 ]
                )
            )
        )


decodeKnownTypeConstructor ctx typeName argsTypes =
    let
        firstType =
            Dict.get typeName ctx.knownTypes
                |> Result.fromMaybe ("Unknown type somehow leaked to transformation stage." ++ TypeName.toStr typeName)
                |> Result.map Variable
    in
        case argsTypes of
            [] ->
                firstType

            l ->
                List.foldl
                    (\item accum ->
                        let
                            genRes =
                                (decodeType ctx item)
                        in
                            Result.map2 Application accum genRes
                    )
                    firstType
                    l


tupleFieldDec ctx ( index, type_ ) =
    Result.map
        (\genRes ->
            Application (variable ctx.decoderPrefix "custom")
                (Application (Application (variable ctx.decoderPrefix "index") (Integer index)) genRes)
        )
        (decodeType ctx type_)


getMapFun n =
    case n of
        1 ->
            "map"

        x ->
            "map" ++ (toString x)
