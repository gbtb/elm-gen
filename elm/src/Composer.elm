module Composer exposing (..)

import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder, defaultContext, genMaybeDecoder)
import Printer exposing (printStatement)
import PrintRepr exposing (PrintRepr(..), produceString, (+>))
import Dependency exposing (..)
import List.Extra as List
import Set
import Dict
import Utils exposing (..)


type alias LocalContext =
    { typesDict : Dict.Dict String Statement
    , graph : Dict.Dict String (Set.Set String)
    , userDefinedTypes : Dict.Dict String (List String)
    }


composeFile : List Statement -> String
composeFile statements =
    let
        types =
            List.filter typesFilter statements

        moduleDeclaration =
            List.find moduleDeclarationFilter statements |> fromJust "Cannot find module declaration!"

        moduleName =
            getModuleName moduleDeclaration

        ( graphHeads, dependencyGraph ) =
            makeDependencyGraph knownTypes types

        typesDict =
            makeTypesDict types

        userDefinedTypes =
            Dict.values dependencyGraph
                |> List.foldl Set.union Set.empty
                |> Set.toList
                |> makeDecodersNameMapping
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| makeDecodersModuleDecl moduleDeclaration
                , emptyLine
                ]
                    ++ printImports moduleName (getTypes statements)
                    ++ (traverseDepGraphAndPrintDecoders
                            (LocalContext typesDict
                                dependencyGraph
                                userDefinedTypes
                            )
                            graphHeads
                       )
                    ++ [ emptyLine ]


traverseDepGraphAndPrintDecoders context graphHeads =
    List.concatMap
        (\head -> printDecoders context head)
        (Set.toList graphHeads)


getTypes =
    List.filterMap
        (\s ->
            case s of
                TypeAliasDeclaration (TypeConstructor [ consName ] []) _ ->
                    Just <| (flip TypeExport) Nothing consName

                TypeDeclaration (TypeConstructor [ consName ] []) _ ->
                    Just <| (flip TypeExport) Nothing consName

                _ ->
                    Nothing
        )



--printDecoders graph types


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


makeDecodersNameMapping types =
    List.map (\type_ -> ( type_, [ String.toLower type_ ++ "Decoder" ] )) types |> Dict.fromList


printDecoders localContext key =
    let
        listOfDecoders =
            printDecodersHelper localContext key

        printedDecoders =
            List.map (\decoderDecl -> [ emptyLine, emptyLine ] ++ List.map printStatement decoderDecl) listOfDecoders
    in
        List.concat printedDecoders


printDecodersHelper : LocalContext -> String -> List (List Statement)
printDecodersHelper localContext item =
    let
        dfsDecoders =
            Set.foldl
                (\key list -> List.append list <| printDecodersHelper localContext key)
                []
            <|
                fromJust ("Types set not found in deps graph: " ++ item) <|
                    Dict.get item localContext.graph
    in
        dfsDecoders
            ++ [ if item == "Maybe" then
                    [ genMaybeDecoder ]
                 else
                    genDecoder (Transformation.initContext "JD" localContext.userDefinedTypes) <|
                        fromJust "Type not found in types dict!" <|
                            Dict.get item localContext.typesDict
               ]


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
