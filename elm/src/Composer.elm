module Composer exposing (..)

import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder, defaultContext, genMaybeDecoder, genMaybeEncoder, TransformationContext, genEncoder, genEncoderForMappable)
import Printer exposing (printStatement)
import PrintRepr exposing (PrintRepr(..), produceString, (+>))
import Dependency exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Set
import Dict
import Utils exposing (..)
import Model exposing (..)
import Config exposing (..)
import StatementFilters exposing (..)
import ReadConfig exposing (..)


type alias GenContext =
    { typesDict : Dict.Dict String Statement
    , graph : Dict.Dict String TypeSet
    , userDefinedTypes : Dict.Dict String (List String)
    , excludedTypes : TypeSet
    , generatorFunc : TransformationContext -> Statement -> List Statement
    , prefix : String
    , makeName : String -> String
    , isDecoders : Bool
    , maybeStub : Statement
    , mappableStubs : Dict.Dict String (List Statement)
    }


decTcName =
    [ "JD", "Decoder" ]


encTcName =
    [ "JE", "Value" ]


mappableStubs ctx =
    Dict.fromList <| List.map (\i -> ( i, genEncoderForMappable ctx i )) [ "List", "Array" ]


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

        statements =
            if not firstCall then
                model.newlyParsedStatements
            else
                model.parsedStatements

        types =
            List.filter (extractType >> asFilter) <| statements

        decoders =
            Dict.fromList <|
                List.filterMap (extractDecoder decTcName) <|
                    statements

        encoders =
            Dict.fromList <|
                List.filterMap (extractEncoder encTcName) <|
                    statements

        moduleName =
            List.find (extractModuleDeclaration >> asFilter) statements
                |> Maybe.andThen extractModuleDeclaration
                |> fromJust "Module declaration not found!"

        importsDict =
            Dict.fromList [ ( moduleName, Set.fromList <| getTypes model.genCommand decoders encoders unknownTypes statements ) ]

        typesDict =
            Dict.union model.typesDict <|
                (makeTypesDict types)

        ( oldGraphHeads, oldGraph ) =
            model.dependencies

        ( newGraphHeads, newGraph ) =
            makeDependencyGraph (List.foldl Set.union Set.empty <| Dict.values oldGraph)
                (knownTypes)
                types

        ( graphHeads, graph ) =
            ( Set.union newGraphHeads oldGraphHeads, Dict.union oldGraph newGraph )

        userDefinedTypes =
            Dict.values graph
                |> List.foldl Set.union Set.empty

        unknownTypes =
            Set.diff (userDefinedTypes) (Set.fromList <| Dict.keys typesDict) |> (\s -> Set.diff s <| Set.fromList [ "Maybe", "List", "Array" ])
    in
        { model
            | typesDict = typesDict
            , unknownTypes = unknownTypes
            , dependencies = ( graphHeads, graph )
            , newlyParsedStatements = []
            , providedDecoders = Dict.union decoders model.providedDecoders
            , providedEncoders = Dict.union encoders model.providedEncoders
            , importsDict =
                Dict.union importsDict model.importsDict
            , parsedStatements = model.parsedStatements ++ model.newlyParsedStatements
        }


generate : Model -> Model
generate model =
    let
        moduleDeclaration =
            List.find (extractModuleDeclaration >> asFilter) model.parsedStatements |> fromJust "Cannot find module declaration!"

        ( graphHeads, graph ) =
            model.dependencies

        userDefinedTypesDecoders =
            Dict.values graph
                |> List.foldl Set.union Set.empty
                |> setdiff (Set.fromList [ "List", "Array" ])
                |> Set.toList
                |> makeNameMapping (getNameFunc model.config.decodersName)
                |> Dict.union (Dict.map (\_ v -> [ v ]) model.providedDecoders)

        userDefinedTypesEncoders =
            Dict.values graph
                |> List.foldl Set.union Set.empty
                |> Set.toList
                |> makeNameMapping (getNameFunc model.config.encodersName)
                |> Dict.union (Dict.map (\_ v -> [ v ]) model.providedEncoders)
    in
        { model
            | moduleDeclaration = moduleDeclaration
            , generatedDecoders =
                if (model.genCommand == Decoders) || (model.genCommand == DecodersAndEncoders) then
                    let
                        nameFunc =
                            (getNameFunc model.config.decodersName)
                    in
                        generateDecoders
                            (GenContext model.typesDict
                                graph
                                (userDefinedTypesDecoders)
                                (keysSet model.providedDecoders)
                                genDecoder
                                "JD"
                                nameFunc
                                True
                                (genMaybeDecoder nameFunc)
                                Dict.empty
                            )
                            graphHeads
                else
                    []
            , generatedEncoders =
                if (model.genCommand == Encoders) || (model.genCommand == DecodersAndEncoders) then
                    let
                        nameFunc =
                            (getNameFunc model.config.encodersName)
                    in
                        generateDecoders
                            (GenContext model.typesDict
                                graph
                                userDefinedTypesEncoders
                                (keysSet model.providedEncoders)
                                genEncoder
                                "JE"
                                nameFunc
                                False
                                (genMaybeEncoder nameFunc)
                                (mappableStubs
                                    { decoderPrefix = "JE" }
                                )
                            )
                            graphHeads
                else
                    []
        }


composeFile : Model -> String
composeFile model =
    let
        moduleName =
            extractModuleDeclaration model.moduleDeclaration
                |> fromJust "Cannot extract input file module declaration!"
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| ModuleDeclaration (getModuleNameFromOutputFileName moduleName model.outputFileName) AllExport
                , emptyLine
                ]
                    ++ printImports model.importsDict model.typesDict
                    ++ (printDecoders model.generatedDecoders)
                    ++ (printDecoders model.generatedEncoders)
                    ++ [ emptyLine ]


printDecoders decoders =
    if decoders == [] then
        []
    else
        List.concatMap (\decoderDecl -> [ emptyLine, emptyLine ] ++ List.map printStatement decoderDecl) decoders


getTypes : GenCommand -> Dict.Dict String String -> Dict.Dict String String -> Set.Set String -> List Statement -> List String
getTypes genCommand decoders encoders unknownTypes lst =
    List.sort <|
        List.concat <|
            List.filterMap
                (\s ->
                    extractType s
                        |> Maybe.map
                            (\( consName, _ ) ->
                                let
                                    decoder =
                                        Dict.get consName decoders

                                    encoder =
                                        Dict.get consName encoders

                                    typeItself =
                                        if
                                            (Maybe.isNothing decoder && willGenDecoder genCommand)
                                                || (Maybe.isNothing encoder && willGenEncoder genCommand)
                                        then
                                            (if Set.member consName unknownTypes then
                                                Nothing
                                             else
                                                Just consName
                                            )
                                        else
                                            Nothing
                                in
                                    List.filterMap identity [ decoder, encoder, typeItself ]
                            )
                )
                lst


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


makeNameMapping nameFunc types =
    List.map (\type_ -> ( type_, [ getDecoderName type_ nameFunc ] )) types |> Dict.fromList


generateDecoders genContext graphHeads =
    let
        excludeTypes =
            Set.union genContext.excludedTypes <|
                if genContext.isDecoders then
                    (Set.fromList [ "List", "Array" ])
                else
                    Set.empty

        typesList =
            Set.toList <| setdiff excludeTypes <| List.foldl Set.union graphHeads <| Dict.values <| genContext.graph
    in
        List.map (generateDecodersHelper genContext) typesList |> Maybe.values


generateDecodersHelper : GenContext -> String -> Maybe (List Statement)
generateDecodersHelper genContext item =
    if item == "Maybe" then
        Just [ genContext.maybeStub ]
    else
        Maybe.orLazy (Dict.get item genContext.mappableStubs)
            (\_ ->
                let
                    typeDeclaration =
                        Dict.get item genContext.typesDict
                in
                    case typeDeclaration of
                        Just stmt ->
                            Just <|
                                genContext.generatorFunc
                                    (Transformation.initContext
                                        genContext.isDecoders
                                        genContext.prefix
                                        genContext.makeName
                                        genContext.userDefinedTypes
                                    )
                                    stmt

                        Nothing ->
                            Debug.crash ("Type not found in types dict! " ++ item ++ (toString genContext.mappableStubs))
            )


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


printImports importsDict typesDict =
    let
        defaultImports =
            [ ImportStatement [ "Json", "Decode" ] (Just "JD") (Nothing)
            , ImportStatement [ "Json", "Decode", "Pipeline" ] (Just "JD") (Nothing)
            , ImportStatement [ "Json", "Encode" ] (Just "JE") (Nothing)
            ]

        toExport ts =
            ts |> Set.toList |> List.map (\name -> TypeExport name (getExportSet typesDict name)) |> SubsetExport |> Just

        typesImports =
            List.map (\( moduleName, typeSet ) -> ImportStatement moduleName Nothing (toExport typeSet)) <|
                Dict.toList
                    importsDict
    in
        List.map printStatement (defaultImports ++ typesImports)


getExportSet typesDict name =
    let
        isUnionType =
            Dict.get name typesDict |> Maybe.andThen extractType |> Maybe.map Tuple.second |> Maybe.withDefault False
    in
        if isUnionType then
            Just AllExport
        else
            Nothing


emptyLine =
    Line 0 ""
