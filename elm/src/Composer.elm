module Composer exposing (..)

import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder, defaultContext)
import Printer exposing (printStatement)
import PrintRepr exposing (PrintRepr(..), produceString, (+>))
import Dependency exposing (..)
import List.Extra as List
import Set
import Dict
import Utils exposing (..)


composeFile : List Statement -> String
composeFile statements =
    let
        types =
            List.filter typesFilter statements

        moduleDeclaration =
            List.find moduleDeclarationFilter statements |> fromJust "Cannot find module declaration!"

        moduleName =
            getModuleName moduleDeclaration

        dependencyGraph =
            makeDependencyGraph knownTypes types

        typesDict =
            makeTypesDict types
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| makeDecodersModuleDecl moduleDeclaration
                , emptyLine
                ]
                    ++ printImports moduleName (getTypes statements)
                    ++ [ emptyLine, emptyLine ]
                    ++ printDecoders typesDict
                        dependencyGraph
                        (Dict.keys dependencyGraph |> List.head |> fromJust "File has no types to made decoders for!")
                    ++ [ emptyLine ]


getTypes =
    List.filterMap
        (\s ->
            case s of
                TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
                    Just <| (flip TypeExport) Nothing consName

                _ ->
                    Nothing
        )



--printDecoders graph types


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


printDecoders typesDict graph key =
    List.map (printStatement) <| List.concat <| printDecodersHelper typesDict graph key []


printDecodersHelper typesDict graph item decodersList =
    let
        dfsDecoders =
            Set.foldl (printDecodersHelper typesDict graph) [] <| fromJust "Types set not found in deps graph" <| Dict.get item graph
    in
        decodersList ++ dfsDecoders ++ [ genDecoder defaultContext <| fromJust "Type not found in types dict!" <| Dict.get item typesDict ]



--printDecoders types =
--    List.concatMap (genDecoder defaultContext) types |> List.map printStatement


getModuleName s =
    case s of
        ModuleDeclaration m _ ->
            m

        _ ->
            Debug.crash "Not a module name!"


printImports sourceModuleName importedTypes =
    [ ImportStatement [ "Json", "Decode" ] (Just "JD") (Nothing)
    , ImportStatement [ "Json", "Decode", "Pipeline" ] (Just "JD") (Nothing)
    , ImportStatement [ "Json", "Encode" ] (Just "JE") (Nothing)
    , ImportStatement sourceModuleName (Nothing) (Just <| SubsetExport importedTypes)
    ]
        |> List.map (printStatement)


makeDecodersModuleDecl stmt =
    case stmt of
        ModuleDeclaration moduleName _ ->
            let
                newModuleName =
                    List.updateAt (List.length moduleName - 1) (\x -> x ++ "Decoders") moduleName
                        |> fromJust "Impossibru!"
            in
                ModuleDeclaration newModuleName AllExport

        _ ->
            Debug.crash "Incorrect statement kind was passed!"


typesFilter s =
    case s of
        TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
            True

        TypeDeclaration _ _ ->
            True

        _ ->
            False


moduleDeclarationFilter s =
    True


emptyLine =
    Line 0 ""
