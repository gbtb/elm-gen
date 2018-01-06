module Transformation exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Set
import Dict
import Char
import Utils exposing (..)


type alias TransformationContext =
    { decoderPrefix : String
    , knownTypes : Dict.Dict String (List String)
    }


defaultContext =
    initContext "JD" Dict.empty


initContext prefix userDefinedTypes =
    let
        basicTypes =
            [ "Int", "Float", "String", "List", "Array", "Char", "Bool" ]

        basicTypesDecoders =
            basicTypes
                |> List.map (\type_ -> ( type_, qualifiedName prefix (String.toLower type_) ))
                |> Dict.fromList
    in
        { decoderPrefix = prefix
        , knownTypes = basicTypesDecoders |> Dict.union userDefinedTypes
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

        TypeDeclaration leftPart rightPart ->
            let
                typeName =
                    getTypeName leftPart

                decoderName =
                    getDecoderName typeName

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


qualifiedName prefix name =
    if String.length prefix > 0 then
        [ prefix, name ]
    else
        [ name ]


genDecoderForUnionType : TransformationContext -> Statement -> Expression
genDecoderForUnionType ctx unionType =
    let
        begin =
            Application (variable ctx.decoderPrefix "oneOf")
    in
        case unionType of
            TypeDeclaration (TypeConstructor [ typeName ] []) constructors ->
                begin <| List <| List.map (genDecoderForUnionTypeConstructor ctx) constructors

            _ ->
                Debug.crash "It is not a union type!"


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
                (Application
                    (variable ctx.decoderPrefix <| getMapFun n)
                    (variable "" name)
                )
    in
        case List.reverse args of
            [] ->
                (Application (variable ctx.decoderPrefix "succeed") (variable "" name))

            a :: cons ->
                start <|
                    List.foldl (\item accum -> Application (decodeType ctx item) accum) (decodeType ctx a) cons


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


genMaybeDecoder =
    FunctionDeclaration "maybeDecoder"
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


getDecoderName typeName =
    case String.uncons typeName of
        Just ( h, tail ) ->
            String.cons (Char.toLower h) (tail ++ "Decoder")

        Nothing ->
            ""
