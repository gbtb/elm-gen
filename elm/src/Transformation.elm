module Transformation exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Set
import Dict
import Char
import Utils exposing (..)
import List.Extra as List
import ReadConfig exposing (..)


type alias TransformationContext =
    { decoderPrefix : String
    , assumeUnionTypeDefaultConstructor : Bool
    , knownTypes : Dict.Dict String (List String)
    , makeName : String -> String
    , defaultRecordValues : Dict.Dict ( String, String ) Expression
    , defaultUnionValues : Dict.Dict String Expression
    }


defaultContext isDecoders =
    initContext isDecoders "JD" defaultDecoderNameFunc Dict.empty


knownTypesForDecoders prefix =
    [ "Int", "Float", "String", "List", "Array", "Char", "Bool" ]
        |> List.map (\type_ -> ( type_, qualifiedName prefix (String.toLower type_) ))
        |> Dict.fromList


knownTypesForEncoders prefix nameFunc =
    ([ "Int", "Float", "String", "Char", "Bool" ]
        |> List.map (\type_ -> ( type_, qualifiedName prefix (String.toLower type_) ))
    )
        ++ ([ "List", "Array" ] |> List.map (\type_ -> ( type_, qualifiedName "" (getDecoderName type_ nameFunc) )))
        |> Dict.fromList


initContext isDecoders prefix nameFunc userDefinedTypes d1 d2 =
    { decoderPrefix = prefix
    , assumeUnionTypeDefaultConstructor = False
    , knownTypes =
        (if isDecoders then
            knownTypesForDecoders prefix
         else
            knownTypesForEncoders prefix nameFunc
        )
            |> Dict.union userDefinedTypes
    , makeName = nameFunc
    , defaultRecordValues = d1
    , defaultUnionValues = d2
    }


genDecoder : TransformationContext -> Statement -> List Statement
genDecoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    getDecoderName typeName context.makeName

                decoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Decoder" ]
                    else
                        [ "Decoder" ]
            in
                [ FunctionTypeDeclaration decoderName <| TypeConstructor decoderType ([ leftPart ])
                , FunctionDeclaration (decoderName) [] <| genDecoderForRecord context typeName rightPart
                ]

        TypeDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    getDecoderName typeName context.makeName

                decoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Decoder" ]
                    else
                        [ "Decoder" ]
            in
                [ FunctionTypeDeclaration decoderName <| TypeConstructor decoderType ([ leftPart ])
                , FunctionDeclaration (decoderName) [] <| genDecoderForUnionType context stmt
                ]

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


genEncoder : TransformationContext -> Statement -> List Statement
genEncoder context stmt =
    case stmt of
        TypeAliasDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    getDecoderName typeName context.makeName

                encoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Value" ]
                    else
                        [ "Value" ]
            in
                [ FunctionTypeDeclaration decoderName <| (TypeApplication (TypeConstructor [ typeName ] [])) (TypeConstructor encoderType [])
                , FunctionDeclaration (decoderName) [ variable "" "value" ] <| genEncoderForRecord context typeName rightPart
                ]

        TypeDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    getDecoderName typeName context.makeName

                encoderType =
                    if String.length context.decoderPrefix > 0 then
                        [ context.decoderPrefix, "Value" ]
                    else
                        [ "Value" ]
            in
                [ FunctionTypeDeclaration decoderName <| (TypeApplication (TypeConstructor [ typeName ] [])) (TypeConstructor encoderType [])
                , FunctionDeclaration (decoderName) [ variable "" "value" ] <| genEncoderForUnionType context stmt
                ]

        _ ->
            Debug.crash "Cannot generate decoder for this kind of statement(yet?)"


qualifiedName prefix name =
    if String.length prefix > 0 then
        [ prefix, name ]
    else
        [ name ]


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


genDecoderForUnionType : TransformationContext -> Statement -> Expression
genDecoderForUnionType ctx unionType =
    let
        begin =
            Application (variable ctx.decoderPrefix "oneOf")
    in
        case unionType of
            TypeDeclaration (TypeConstructor [ typeName ] []) constructors ->
                begin <|
                    List <|
                        addDefaultConstructorDecoder ctx constructors <|
                            List.map (genDecoderForUnionTypeConstructor ctx) constructors

            _ ->
                Debug.crash "It is not a union type!"


addDefaultConstructorDecoder ctx constructors generatedCode =
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


genDecoderForUnionTypeConstructor ctx cons =
    case cons of
        TypeConstructor [ name ] args ->
            Application
                (Application (variable ctx.decoderPrefix "field")
                    (String name)
                )
                (decodeUnionTypeArgs ctx name args)

        _ ->
            Debug.crash "Invalid union type constructor!"


decodeUnionTypeArgs ctx name args =
    let
        n =
            List.length args

        start =
            Application
                (variable ctx.decoderPrefix <| getMapFun n)
                (variable "" name)

        indexAppl idx =
            Application (Application (variable ctx.decoderPrefix "index") (Integer idx))
    in
        case args of
            [] ->
                (Application (variable ctx.decoderPrefix "succeed") (variable "" name))

            [ a ] ->
                Application start (decodeType ctx a)

            l ->
                List.indexedFoldl (\idx item accum -> Application accum (indexAppl idx <| decodeType ctx item)) start l


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


genEncoderForRecord : TransformationContext -> String -> Type -> Expression
genEncoderForRecord ctx typeName recordAst =
    let
        encodeApp =
            (Application (Variable <| qualifiedName ctx.decoderPrefix "decode") (Variable [ typeName ]))
    in
        case recordAst of
            TypeRecord l ->
                Application (Variable <| qualifiedName ctx.decoderPrefix "object") (List <| List.map (encodeRecordField ctx) l)

            _ ->
                Debug.crash "It is not a record!"


encodeRecordField ctx ( name, type_ ) =
    let
        typeEncoder ctx =
            encodeType ctx type_
    in
        Tuple [ (String name), (Application <| typeEncoder ctx) (Access (variable "" "value") [ name ]) ]


recordFieldDec ctx ( name, type_ ) =
    let
        typeDecoder ctx =
            decodeType ctx type_
    in
        Application (Application (Variable <| qualifiedName ctx.decoderPrefix "required") (String name)) (typeDecoder ctx)


encodeType ctx type_ =
    case type_ of
        TypeConstructor [ typeName ] argsTypes ->
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
                |> fromJust ("Unknown type somehow leaked to transformation stage." ++ typeName)
                |> Variable
    in
        case argsTypes of
            [] ->
                firstType

            l ->
                List.foldl (\item accum -> Application accum (decodeType ctx item)) firstType l


decodeType ctx type_ =
    case type_ of
        TypeConstructor [ typeName ] argsTypes ->
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
            ((String.toLower typeName) ++ "Encoder")

        value =
            TypeConstructor (qualifiedName ctx.decoderPrefix "Value") []
    in
        [ FunctionTypeDeclaration funcName
            (TypeApplication
                (TypeApplication (TypeVariable "a")
                    (value)
                )
                (TypeApplication (TypeConstructor [ typeName ] ([ TypeVariable "a" ])) (value))
            )
        , FunctionDeclaration funcName
            ([ Variable [ "encoder" ], Variable [ "value" ] ])
            (BinOp (Variable [ "<|" ])
                (variable ctx.decoderPrefix (String.toLower typeName))
                (Application (Application (Access (variable "" typeName) [ "map" ]) (Variable [ "encoder" ])) (Variable [ "value" ]))
            )
        ]


decodeKnownTypeConstructor ctx typeName argsTypes =
    let
        firstType =
            Dict.get typeName ctx.knownTypes
                |> fromJust ("Unknown type somehow leaked to transformation stage." ++ typeName)
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


variable prefix x =
    Variable <| qualifiedName prefix x


pipeOp =
    BinOp (Variable [ "|>" ])


getMapFun n =
    case n of
        1 ->
            "map"

        x ->
            "map" ++ (toString x)


getDummyVariables n =
    (List.range 1 n) |> List.map toString |> List.map ((++) "v") |> List.map (variable "")
