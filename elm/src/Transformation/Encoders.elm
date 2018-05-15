module Transformation.Encoders exposing (..)

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


genEncoder : TransformationContext -> Statement -> List Statement
genEncoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart
            in
                genEncoderHelper context leftPart rightPart (genEncoderForRecord context typeName rightPart)

        TypeDeclaration leftPart rightPart ->
            genEncoderHelper context leftPart rightPart (genEncoderForUnionType context stmt)

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


genEncoderHelper context leftPart rightPart generatorInvocation =
    let
        typeName =
            getTypeName leftPart

        decoderName =
            TypeName.getDecoderName typeName context.makeName

        encoderType =
            if String.length context.decoderPrefix > 0 then
                [ context.decoderPrefix, "Value" ]
            else
                [ "Value" ]

        encoderDeclaration =
            FunctionTypeDeclaration decoderName <| (TypeApplication (TypeConstructor typeName [])) (TypeConstructor encoderType [])

        encoderBody =
            FunctionDeclaration (decoderName) [ variable "" "value" ] <| generatorInvocation
    in
        if Set.member typeName context.dontDeclareTypes then
            List.singleton encoderBody
        else
            [ encoderDeclaration, encoderBody ]


genEncoderForUnionType : TransformationContext -> Statement -> Expression
genEncoderForUnionType ctx unionType =
    let
        begin =
            Case (variable "" "value")
    in
        case unionType of
            TypeDeclaration (TypeConstructor [ typeName ] []) constructors ->
                begin <| List.map (genEncoderForUnionTypeConstructor ctx) constructors

            _ ->
                Debug.crash "It is not a union type!"


genEncoderForUnionTypeConstructor ctx cons =
    case cons of
        TypeConstructor [ name ] args ->
            let
                n =
                    List.length args

                start =
                    variable "" name
            in
                ( if n == 0 then
                    start
                  else
                    List.foldl (\item accum -> Application accum item) start (getDummyVariables n)
                , (Application (variable ctx.decoderPrefix "object")
                    (List <| [ encodeUnionTypeArgs ctx name args ])
                  )
                )

        _ ->
            Debug.crash "Invalid union type constructor!"


encodeUnionTypeArgs ctx name args =
    let
        n =
            List.length args

        tup expr =
            Tuple [ String name, expr ]
    in
        case args of
            [] ->
                tup <| variable ctx.decoderPrefix "null"

            [ a ] ->
                tup <| Application (encodeType ctx a) (variable "" "v1")

            l ->
                tup <|
                    Application (variable ctx.decoderPrefix "list") <|
                        List <|
                            List.map (\( type_, var ) -> Application (encodeType ctx type_) var) (List.zip l <| getDummyVariables n)


genEncoderForRecord : TransformationContext -> TypeName -> Type -> Expression
genEncoderForRecord ctx typeName recordAst =
    let
        encodeApp =
            (Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable typeName))
    in
        case recordAst of
            TypeRecord l ->
                Application (Variable <| qualifiedName ctx.decoderPrefix "object") (List <| List.map (encodeRecordField ctx) l)

            TypeConstructor _ _ ->
                Case (variable "" "value") <| List.singleton <| genEncoderForUnionTypeConstructor ctx recordAst

            _ ->
                Debug.crash "It is not a record!"


encodeRecordField ctx ( name, type_ ) =
    let
        typeEncoder ctx =
            encodeType ctx type_
    in
        Tuple [ (String name), (Application <| typeEncoder ctx) (Access (variable "" "value") [ name ]) ]


encodeType ctx type_ =
    case type_ of
        TypeConstructor typeName argsTypes ->
            encodeKnownTypeConstructor ctx typeName argsTypes

        TypeTuple [ a ] ->
            (encodeType ctx a)

        TypeTuple l ->
            case List.reverse <| List.indexedMap (\i x -> ( i, x )) l of
                [] ->
                    Debug.crash "Empty TypeTuple is not allowed!"

                _ ->
                    Debug.crash "Op"

        _ ->
            Debug.crash "Cannot encode this type yet?"


encodeKnownTypeConstructor ctx typeName argsTypes =
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
                List.foldl (\item accum -> Application accum (encodeType ctx item)) firstType l


genMaybeEncoder nameFunc =
    FunctionDeclaration (nameFunc "maybe")
        ([ Variable [ "valueEncoder" ], Variable [ "value" ] ])
        (Case (Variable [ "value" ])
            ([ ( Application (Variable [ "Just" ]) (Variable [ "value" ])
               , Application (Variable [ "valueEncoder" ]) (Variable [ "value" ])
               )
             , ( Variable [ "Nothing" ], Access (Variable [ "JE" ]) [ "null" ] )
             ]
            )
        )


genEncoderForMappable ctx typeName =
    let
        funcName =
            ((TypeName.toLowerCaseName typeName) ++ "Encoder")

        value =
            TypeConstructor (qualifiedName ctx.decoderPrefix "Value") []
    in
        [ FunctionTypeDeclaration funcName
            (TypeApplication
                (TypeApplication (TypeVariable "a")
                    (value)
                )
                (TypeApplication (TypeConstructor typeName ([ TypeVariable "a" ])) (value))
            )
        , FunctionDeclaration funcName
            ([ Variable [ "encoder" ], Variable [ "value" ] ])
            (BinOp (Variable [ "<|" ])
                (variable ctx.decoderPrefix (TypeName.toLowerCaseName typeName))
                (Application (Application (Access (Variable typeName) [ "map" ]) (Variable [ "encoder" ])) (Variable [ "value" ]))
            )
        ]


getDummyVariables n =
    (List.range 1 n) |> List.map toString |> List.map ((++) "v") |> List.map (variable "")
