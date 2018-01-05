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
import Model exposing (..)


type alias GenContext =
    { typesDict : Dict.Dict String Statement
    , graph : Dict.Dict String (Set.Set String)
    , userDefinedTypes : Dict.Dict String (List String)
    }


makeFileLoadRequest : Model -> Result String (List (List String))
makeFileLoadRequest model =
    let
        imports =
            List.filter importsFilter model.parsedStatements

        ( unknownTypes, filesList ) =
            List.foldl importFoldHelper ( model.unknownTypes, [] ) imports
    in
        if not <| Set.isEmpty unknownTypes then
            Err <| "Cannot find direct import(s) of type(s): [" ++ String.join ", " (Set.toList unknownTypes) ++ "] in import statements!"
        else
            Ok <| List.reverse filesList


resolveDependencies : Model -> Model
resolveDependencies model =
    let
        types =
            List.filter typesFilter model.parsedStatements

        ( graphHeads, graph ) =
            makeDependencyGraph knownTypes types

        typesDict =
            makeTypesDict types

        userDefinedTypes =
            Dict.values graph
                |> List.foldl Set.union Set.empty

        unknownTypes =
            Set.diff (userDefinedTypes) (Set.fromList <| Dict.keys typesDict)

        ( oldGraphHeads, oldGraph ) =
            model.dependencies
    in
        { model
            | typesDict = Dict.union model.typesDict typesDict
            , unknownTypes = Set.union model.unknownTypes unknownTypes
            , dependencies = ( Set.union oldGraphHeads graphHeads, Dict.union oldGraph graph )
        }


generate : Model -> Model
generate model =
    let
        types =
            List.filter typesFilter model.parsedStatements

        moduleDeclaration =
            List.find moduleDeclarationFilter model.parsedStatements |> fromJust "Cannot find module declaration!"

        ( graphHeads, graph ) =
            makeDependencyGraph knownTypes types

        typesDict =
            makeTypesDict types

        userDefinedTypes =
            Dict.values graph
                |> List.foldl Set.union Set.empty
                |> Set.toList
                |> makeDecodersNameMapping
    in
        { model
            | moduleDeclaration = moduleDeclaration
            , generatedDecoders =
                traverseDepGraphAndGenerateDecoders
                    (GenContext typesDict
                        graph
                        userDefinedTypes
                    )
                    graphHeads
        }


composeFile : Model -> String
composeFile model =
    let
        moduleName =
            getModuleName model.moduleDeclaration
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| makeDecodersModuleDecl model.moduleDeclaration
                , emptyLine
                ]
                    ++ printImports moduleName (getTypes model.parsedStatements)
                    ++ (printDecoders model.generatedDecoders)
                    ++ [ emptyLine ]


printDecoders decoders =
    List.concatMap (\decoderDecl -> [ emptyLine, emptyLine ] ++ List.map printStatement decoderDecl) decoders


traverseDepGraphAndGenerateDecoders context graphHeads =
    List.concatMap
        (\head -> generateDecoders context head)
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


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


makeDecodersNameMapping types =
    List.map (\type_ -> ( type_, [ String.toLower type_ ++ "Decoder" ] )) types |> Dict.fromList


generateDecoders genContext key =
    let
        listOfDecoders =
            generateDecodersHelper genContext key
    in
        listOfDecoders


generateDecodersHelper : GenContext -> String -> List (List Statement)
generateDecodersHelper genContext item =
    let
        dfsDecoders =
            Set.foldl
                (\key list -> List.append list <| generateDecodersHelper genContext key)
                []
            <|
                fromJust ("Types set not found in deps graph: " ++ item) <|
                    Dict.get item genContext.graph
    in
        dfsDecoders
            ++ [ if item == "Maybe" then
                    [ genMaybeDecoder ]
                 else
                    genDecoder (Transformation.initContext "JD" genContext.userDefinedTypes) <|
                        fromJust "Type not found in types dict!" <|
                            Dict.get item genContext.typesDict
               ]


getModuleName s =
    case s of
        ModuleDeclaration m _ ->
            m

        _ ->
            Debug.crash "Not a module name!"


importFoldHelper : Statement -> ( Set.Set String, List (List String) ) -> ( Set.Set String, List (List String) )
importFoldHelper importStmt ( unknownTypes, filesList ) =
    let
        defaultReturn =
            ( unknownTypes, filesList )
    in
        if Set.isEmpty unknownTypes then
            defaultReturn
        else
            case importStmt of
                ImportStatement moduleName _ mbExportSet ->
                    case mbExportSet of
                        Just exportSet ->
                            let
                                importedTypes =
                                    Set.fromList <| getImportedTypes exportSet
                            in
                                if not <| Set.isEmpty <| Set.intersect importedTypes unknownTypes then
                                    ( Set.diff unknownTypes importedTypes, moduleName :: filesList )
                                else
                                    defaultReturn

                        Nothing ->
                            defaultReturn

                _ ->
                    Debug.crash "Non-import statement passed to function!"


getImportedTypes : ExportSet -> List String
getImportedTypes exportSet =
    case exportSet of
        SubsetExport listOfExports ->
            List.foldl (\exp list -> list ++ getImportedTypes exp) [] listOfExports

        TypeExport typeName _ ->
            [ typeName ]

        _ ->
            []


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
    case s of
        ModuleDeclaration m _ ->
            True

        _ ->
            False


importsFilter s =
    case s of
        ImportStatement _ _ _ ->
            True

        _ ->
            False


emptyLine =
    Line 0 ""
