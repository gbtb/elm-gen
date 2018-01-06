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
import StatementFilters exposing (..)


type alias GenContext =
    { typesDict : Dict.Dict String Statement
    , graph : Dict.Dict String TypeSet
    , userDefinedTypes : Dict.Dict String (List String)
    }


makeFileLoadRequest : Model -> Result String (Dict.Dict (List String) TypeSet)
makeFileLoadRequest model =
    let
        imports =
            List.filter (extractImport >> asFilter) model.parsedStatements

        ( unknownTypes, modulesDict ) =
            List.foldl importFoldHelper ( model.unknownTypes, Dict.empty ) imports
    in
        if not <| Set.isEmpty unknownTypes then
            Err <| "Cannot find direct import(s) of type(s): [" ++ String.join ", " (Set.toList unknownTypes) ++ "] in import statements!"
        else
            Ok <| modulesDict


{-| This function is designed to handle additional loading of type definitions came through fileLoadRequest
 as well as initial loading of provided input file(s)
-}
resolveDependencies : Model -> Model
resolveDependencies model =
    let
        firstCall =
            List.length model.newlyParsedStatements == 0

        types =
            List.filter (extractType >> asFilter) <|
                if not firstCall then
                    model.newlyParsedStatements
                else
                    model.parsedStatements

        decoders =
            List.filter (extractDecoder >> asFilter) <|
                if not firstCall then
                    model.newlyParsedStatements
                else
                    model.parsedStatements

        moduleName =
            List.find (extractModuleDeclaration >> asFilter) model.parsedStatements
                |> Maybe.andThen extractModuleDeclaration
                |> fromJust "Module declaration not found!"

        importsDict =
            Dict.fromList [ ( moduleName, Set.fromList <| getTypes unknownTypes model.parsedStatements ) ]

        typesDict =
            Dict.union model.typesDict <| makeTypesDict types

        ( oldGraphHeads, oldGraph ) =
            model.dependencies

        ( newGraphHeads, newGraph ) =
            makeDependencyGraph (List.foldl Set.union Set.empty <| Dict.values oldGraph) knownTypes types

        ( graphHeads, graph ) =
            ( Set.union newGraphHeads oldGraphHeads, Dict.union oldGraph newGraph )

        userDefinedTypes =
            Dict.values graph
                |> List.foldl Set.union Set.empty

        unknownTypes =
            Set.diff (userDefinedTypes) (Set.fromList <| Dict.keys typesDict) |> Set.remove "Maybe"
    in
        { model
            | typesDict = typesDict
            , unknownTypes = unknownTypes
            , dependencies = ( graphHeads, graph )
            , newlyParsedStatements = []
            , importsDict =
                if firstCall then
                    importsDict
                else
                    model.importsDict
            , parsedStatements = model.parsedStatements ++ model.newlyParsedStatements
        }


generate : Model -> Model
generate model =
    let
        moduleDeclaration =
            List.find (extractModuleDeclaration >> asFilter) model.parsedStatements |> fromJust "Cannot find module declaration!"

        ( graphHeads, graph ) =
            model.dependencies

        userDefinedTypes =
            Dict.values graph
                |> List.foldl Set.union Set.empty
                |> Set.toList
                |> makeDecodersNameMapping
    in
        { model
            | moduleDeclaration = moduleDeclaration
            , generatedDecoders =
                generateDecoders
                    (GenContext model.typesDict
                        graph
                        userDefinedTypes
                    )
                    graphHeads
        }


composeFile : Model -> String
composeFile model =
    let
        moduleName =
            extractModuleDeclaration model.moduleDeclaration
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| makeDecodersModuleDecl model.moduleDeclaration
                , emptyLine
                ]
                    ++ printImports model.importsDict
                    ++ (printDecoders model.generatedDecoders)
                    ++ [ emptyLine ]


printDecoders decoders =
    List.concatMap (\decoderDecl -> [ emptyLine, emptyLine ] ++ List.map printStatement decoderDecl) decoders


getTypes unknownTypes =
    List.filterMap
        (\s ->
            extractType s
                |> Maybe.andThen
                    (\consName ->
                        if Set.member consName unknownTypes then
                            Nothing
                        else
                            Just consName
                    )
        )


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


makeDecodersNameMapping types =
    List.map (\type_ -> ( type_, [ getDecoderName type_ ] )) types |> Dict.fromList


generateDecoders genContext graphHeads =
    let
        typesList =
            Set.toList <| List.foldl Set.union graphHeads <| Dict.values <| genContext.graph
    in
        List.map (generateDecodersHelper genContext) typesList


generateDecodersHelper : GenContext -> String -> List Statement
generateDecodersHelper genContext item =
    if item == "Maybe" then
        [ genMaybeDecoder ]
    else
        genDecoder (Transformation.initContext "JD" genContext.userDefinedTypes) <|
            fromJust "Type not found in types dict!" <|
                Dict.get item genContext.typesDict


importFoldHelper : Statement -> ( Set.Set String, Dict.Dict (List String) (Set.Set String) ) -> ( Set.Set String, Dict.Dict (List String) (Set.Set String) )
importFoldHelper importStmt ( unknownTypes, modulesDict ) =
    let
        defaultReturn =
            ( unknownTypes, modulesDict )
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
                                    ( Set.diff unknownTypes importedTypes
                                    , Dict.insert moduleName (Set.intersect unknownTypes importedTypes) modulesDict
                                    )
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


printImports importsDict =
    let
        defaultImports =
            [ ImportStatement [ "Json", "Decode" ] (Just "JD") (Nothing)
            , ImportStatement [ "Json", "Decode", "Pipeline" ] (Just "JD") (Nothing)
            , ImportStatement [ "Json", "Encode" ] (Just "JE") (Nothing)
            ]

        toExport ts =
            ts |> Set.toList |> List.map (\name -> TypeExport name (Nothing)) |> SubsetExport |> Just

        typesImports =
            List.map (\( moduleName, typeSet ) -> ImportStatement moduleName Nothing (toExport typeSet)) <|
                Dict.toList importsDict
    in
        List.map printStatement (defaultImports ++ typesImports)


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


emptyLine =
    Line 0 ""
