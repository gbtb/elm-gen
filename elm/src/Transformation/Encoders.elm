module Transformation.Encoders exposing (encodeKnownTypeConstructor, encodeRecordField, encodeType, encodeUnionTypeArgs, genEncoder, genEncoderForMappable, genEncoderForRecord, genEncoderForTuple, genEncoderForUnionType, genEncoderForUnionTypeConstructor, genEncoderHelper, genMaybeEncoder, getDummyVariables, unionArgsMapHelper)

import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Char
import Config exposing (getEncodePrefix)
import Dict
import List.Extra as List
import Model exposing (TypeName)
import ReadConfig exposing (..)
import Result.Extra as Result
import Set
import Transformation.Shared exposing (..)
import TypeName
import Utils exposing (..)


genEncoder : TransformationContext -> Statement -> Result String (List Statement)
genEncoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeNameRes =
                    getTypeName leftPart
            in
            Result.andThen (genEncoderHelper context leftPart rightPart)
                (Result.andThen (\typeName -> genEncoderForRecord context typeName rightPart) typeNameRes)

        TypeDeclaration leftPart rightPart ->
            Result.andThen (genEncoderHelper context leftPart rightPart) (genEncoderForUnionType context stmt)

        _ ->
            Err "Cannot generate decoder for this kind of statement(yet?)"


genEncoderHelper context leftPart rightPart generatorInvocation =
    let
        typeNameRes =
            getTypeName leftPart
    in
    Result.map
        (\typeName ->
            let
                decoderName =
                    TypeName.getDecoderName typeName context.makeName

                encoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Value" ]

                    else
                        [ "Value" ]

                encoderDeclaration =
                    FunctionTypeDeclaration decoderName <| TypeApplication (TypeConstructor typeName []) (TypeConstructor encoderType [])

                encoderBody =
                    FunctionDeclaration decoderName [ variable "" "value" ] <| generatorInvocation
            in
            if Set.member typeName context.dontDeclareTypes then
                List.singleton encoderBody

            else
                [ encoderDeclaration, encoderBody ]
        )
        typeNameRes


genEncoderForUnionType : TransformationContext -> Statement -> Result String Expression
genEncoderForUnionType ctx unionType =
    let
        begin =
            Case (variable "" "value")
    in
    case unionType of
        TypeDeclaration (TypeConstructor [ typeName ] []) constructors ->
            Result.map begin <| Result.combine <| List.map (genEncoderForUnionTypeConstructor ctx) constructors

        _ ->
            Err "It is not a union type!"


genEncoderForUnionTypeConstructor ctx cons =
    case cons of
        TypeConstructor [ name ] args ->
            let
                n =
                    List.length args

                start =
                    variable "" name

                generationRes =
                    encodeUnionTypeArgs ctx name args
            in
            Result.map
                (\encodedRes ->
                    ( if n == 0 then
                        start

                      else
                        List.foldl (\item accum -> Application accum item) start (getDummyVariables args)
                    , Application (variable ctx.decoderPrefix "object")
                        (List <| [ encodedRes ])
                    )
                )
                generationRes

        _ ->
            Err "Invalid union type constructor!"


encodeUnionTypeArgs ctx name args =
    let
        n =
            List.length args

        tup expr =
            Tuple [ String name, expr ]
    in
    case args of
        [] ->
            Ok <| tup <| variable ctx.decoderPrefix "null"

        [ a ] ->
            Result.map (\genRes -> tup <| Application genRes (variable "" "v1")) (encodeType ctx a)

        l ->
            Result.map
                (\x ->
                    tup <|
                        Application (variable ctx.decoderPrefix "list") <|
                            List x
                )
            <|
                Result.combine <|
                    List.map (unionArgsMapHelper ctx) (List.zip l <| getDummyVariables args)


unionArgsMapHelper ctx ( type_, var ) =
    case var of
        Tuple _ ->
            encodeType ctx type_

        _ ->
            Result.map (\x -> Application x var) (encodeType ctx type_)


genEncoderForRecord : TransformationContext -> TypeName -> Type -> Result String Expression
genEncoderForRecord ctx typeName recordAst =
    let
        encodeApp =
            Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable typeName)
    in
    case recordAst of
        TypeRecord l ->
            Result.map (Application (Variable <| qualifiedName ctx.decoderPrefix "object"))
                (Result.map List <| Result.combine <| List.map (encodeRecordField ctx typeName) l)

        TypeConstructor _ _ ->
            Result.map (\gen -> Application gen (variable "" "value")) (encodeType ctx recordAst)

        _ ->
            Err "It is not a record!"


encodeRecordField ctx typeName ( name, type_ ) =
    let
        typeEncoder ctx =
            encodeType ctx type_

        nameAlias =
            getNameAlias ctx typeName name
    in
    Result.map (\typeEncoder -> Tuple [ String nameAlias, (Application <| typeEncoder) (Access (variable "" "value") [ name ]) ]) (typeEncoder ctx)


encodeType ctx type_ =
    case type_ of
        TypeConstructor typeName argsTypes ->
            encodeKnownTypeConstructor ctx typeName argsTypes

        TypeTuple [ a ] ->
            encodeType ctx a

        TypeTuple l ->
            let
                tupleEncoderName =
                    ctx.makeName ("tuple" ++ (toString <| List.length l))
            in
            case l of
                [] ->
                    Err "Empty TypeTuple is not allowed!"

                args ->
                    List.foldl (\type_ accum -> Result.map2 Application accum (encodeType ctx type_))
                        (Ok <| variable "" tupleEncoderName)
                        args

        _ ->
            Err "Cannot encode this type yet?"


encodeKnownTypeConstructor ctx typeName argsTypes =
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
            List.foldl (\item accum -> Result.map2 Application accum (encodeType ctx item)) firstType l


genMaybeEncoder conf nameFunc =
    FunctionDeclaration (nameFunc "maybe")
        [ Variable [ "valueEncoder" ], Variable [ "valueArg" ] ]
        (Case (Variable [ "valueArg" ])
            [ ( Application (Variable [ "Just" ]) (Variable [ "value" ])
              , Application (Variable [ "valueEncoder" ]) (Variable [ "value" ])
              )
            , ( Variable [ "Nothing" ], Access (Variable [ getEncodePrefix conf ]) [ "null" ] )
            ]
        )


genEncoderForMappable ctx typeName =
    let
        funcName =
            TypeName.getDecoderName typeName ctx.makeName

        value =
            TypeConstructor (qualifiedName ctx.decoderPrefix "Value") []
    in
    [ FunctionTypeDeclaration funcName
        (TypeApplication
            (TypeApplication (TypeVariable "a")
                value
            )
            (TypeApplication (TypeConstructor typeName [ TypeVariable "a" ]) value)
        )
    , FunctionDeclaration funcName
        [ Variable [ "encoder" ], Variable [ "value" ] ]
        (BinOp (Variable [ "<|" ])
            (variable ctx.decoderPrefix (TypeName.toLowerCaseName typeName))
            (Application (Application (Access (Variable typeName) [ "map" ]) (Variable [ "encoder" ])) (Variable [ "value" ]))
        )
    ]


genEncoderForTuple ctx arity typeName =
    let
        funcName =
            TypeName.getDecoderName typeName ctx.makeName

        value =
            TypeConstructor (qualifiedName ctx.decoderPrefix "Value") []

        applicationStart =
            TypeTuple (List.map (\i -> TypeVariable <| "a" ++ toString i) <| List.range 1 (arity + 1))

        tupleArguments =
            List.map (\i -> variable "" <| "t" ++ toString i) <| List.range 1 (arity + 1)

        tupleDestruct =
            Tuple <| tupleArguments

        encArguments =
            List.map (\i -> variable "" <| "e" ++ toString i) <| List.range 1 (arity + 1)
    in
    [ FunctionDeclaration funcName
        (encArguments ++ [ tupleDestruct ])
        (BinOp (Variable [ "|>" ])
            (List <| List.map (\( e, t ) -> Application e t) <| List.zip encArguments tupleArguments)
            (Variable <| qualifiedName ctx.decoderPrefix "list")
        )
    ]


getDummyVariables args =
    List.indexedMap
        (\idx v ->
            variable "" <| "v" ++ toString (idx + 1)
        )
        args
