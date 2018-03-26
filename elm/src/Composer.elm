module Composer exposing (..)

import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
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
import TypeName


type alias GenContext =
    { typesDict : Dict.Dict TypeName Statement
    , graph : Dict.Dict TypeName TypeSet
    , userDefinedTypes : Dict.Dict TypeName (List String)
    , excludedTypes : TypeSet
    , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
    , defaultUnionValues : Dict.Dict TypeName Expression
    , generatorFunc : TransformationContext -> Statement -> List Statement
    , prefix : String
    , makeName : String -> String
    , isDecoders : Bool
    , maybeStub : Statement
    , mappableStubs : Dict.Dict TypeName (List Statement)
    }


decTcName =
    [ "JD", "Decoder" ]


encTcName =
    [ "JE", "Value" ]


mappableTypes =
    List.map TypeName.fromStr [ "List", "Array" ]


mappableStubs ctx =
    Dict.fromList <|
        List.map (\i -> ( i, genEncoderForMappable ctx i )) <|
            mappableTypes


makeFileLoadRequest : Model -> Result String (Dict.Dict (List String) TypeSet)
makeFileLoadRequest model =
    let
        imports =
            List.filter (extractImport >> asFilter) model.parsedStatements

        ( unknownTypes, modulesDict ) =
            List.foldl importFoldHelper ( model.unknownTypes, Dict.empty ) imports
    in
        if not <| Set.isEmpty unknownTypes then
            Err <| "Cannot find direct import(s) of type(s): [" ++ String.join ", " (Set.toList <| Set.map TypeName.toStr unknownTypes) ++ "] in import statements!"
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
            Set.diff (userDefinedTypes) (Set.fromList <| Dict.keys typesDict)
                |> (\s ->
                        Set.diff s <|
                            Set.fromList <|
                                List.map TypeName.fromStr [ "Maybe", "List", "Array" ]
                   )
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
                |> setdiff (Set.fromList mappableTypes)
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
                if willGenDecoder model.genCommand then
                    let
                        nameFunc =
                            (getNameFunc model.config.decodersName)
                    in
                        generateDecoders
                            { typesDict = model.typesDict
                            , graph = graph
                            , userDefinedTypes = userDefinedTypesDecoders
                            , excludedTypes = (keysSet model.providedDecoders)
                            , defaultRecordValues = model.defaultRecordValues
                            , defaultUnionValues = model.defaultUnionValues
                            , generatorFunc = genDecoder
                            , prefix = "JD"
                            , makeName = nameFunc
                            , isDecoders = True
                            , maybeStub = (genMaybeDecoder nameFunc)
                            , mappableStubs = Dict.empty
                            }
                            graphHeads
                else
                    []
            , generatedEncoders =
                if willGenEncoder model.genCommand then
                    let
                        nameFunc =
                            (getNameFunc model.config.encodersName)
                    in
                        generateDecoders
                            { typesDict = model.typesDict
                            , graph = graph
                            , userDefinedTypes = (userDefinedTypesEncoders)
                            , excludedTypes = (keysSet model.providedEncoders)
                            , defaultRecordValues = model.defaultRecordValues
                            , defaultUnionValues = model.defaultUnionValues
                            , generatorFunc = genEncoder
                            , prefix = "JE"
                            , makeName = nameFunc
                            , isDecoders = False
                            , maybeStub = (genMaybeEncoder nameFunc)
                            , mappableStubs =
                                (mappableStubs
                                    { decoderPrefix = "JE" }
                                )
                            }
                            graphHeads
                else
                    []
        }


composeFile : Model -> Result String String
composeFile model =
    let
        moduleName =
            extractModuleDeclaration model.moduleDeclaration
                |> Result.fromMaybe "Cannot extract input file module declaration!"

        moduleDeclaration =
            moduleName
                |> Result.andThen
                    (\moduleName ->
                        getModuleNameFromOutputFileName model.rootDir model.outputFileName
                            |> Result.fromMaybe
                                ("Wrong root directory "
                                    ++ model.rootDir
                                    ++ " and/or output file name "
                                    ++ model.outputFileName
                                    ++ ". Can't generate output module declaration!"
                                )
                    )
    in
        Result.map
            (\moduleDeclaration ->
                String.join "\n" <|
                    List.map (produceString 4) <|
                        [ printStatement <| ModuleDeclaration moduleDeclaration AllExport
                        , emptyLine
                        ]
                            ++ printImports model.genCommand model.importsDict model.typesDict
                            ++ (printDecoders model.generatedDecoders)
                            ++ (printDecoders model.generatedEncoders)
                            ++ [ emptyLine ]
            )
            moduleDeclaration


printDecoders decoders =
    if decoders == [] then
        []
    else
        List.concatMap (\decoderDecl -> [ emptyLine, emptyLine ] ++ List.map printStatement decoderDecl) decoders


getTypes : GenCommand -> Dict.Dict TypeName String -> Dict.Dict TypeName String -> Set.Set TypeName -> List Statement -> List TypeName
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
                                    List.filterMap identity [ Maybe.map TypeName.fromStr decoder, Maybe.map TypeName.fromStr encoder, typeItself ]
                            )
                )
                lst


makeTypesDict types =
    List.foldl (\item accumDict -> Dict.insert (getTypeNameFromStatement item) item accumDict) Dict.empty types


makeNameMapping nameFunc types =
    List.map (\type_ -> ( type_, [ TypeName.getDecoderName type_ nameFunc ] )) types |> Dict.fromList


generateDecoders genContext graphHeads =
    let
        excludeTypes =
            Set.union genContext.excludedTypes <|
                if genContext.isDecoders then
                    (Set.fromList [ TypeName.fromStr "List", TypeName.fromStr "Array" ])
                else
                    Set.empty

        typesList =
            Set.toList <| setdiff excludeTypes <| List.foldl Set.union graphHeads <| Dict.values <| genContext.graph
    in
        List.map (generateDecodersHelper genContext) typesList |> Maybe.values


generateDecodersHelper : GenContext -> TypeName -> Maybe (List Statement)
generateDecodersHelper genContext item =
    if item == TypeName.fromStr "Maybe" then
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
                                        genContext.defaultRecordValues
                                        genContext.defaultUnionValues
                                    )
                                    stmt

                        Nothing ->
                            Debug.crash ("Type not found in types dict! " ++ (TypeName.toStr item) ++ (toString genContext.mappableStubs))
            )


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


getImportedTypes : ExportSet -> List TypeName
getImportedTypes exportSet =
    case exportSet of
        SubsetExport listOfExports ->
            List.foldl (\exp list -> list ++ getImportedTypes exp) [] listOfExports

        TypeExport typeName _ ->
            [ TypeName.fromStr typeName ]

        _ ->
            []


printImports command importsDict typesDict =
    let
        encodersImports =
            [ ImportStatement [ "Json", "Encode" ] (Just "JE") (Nothing) ]

        decodersImports =
            [ ImportStatement [ "Json", "Decode" ] (Just "JD") (Nothing)
            , ImportStatement [ "Json", "Decode", "Pipeline" ] (Just "JD") (Nothing)
            ]

        extImports1 =
            if willGenDecoder command then
                decodersImports
            else
                []

        extImports2 =
            if willGenEncoder command then
                extImports1 ++ encodersImports
            else
                extImports1

        toExport ts =
            ts |> Set.toList |> List.map (toExportHelper typesDict) |> SubsetExport |> Just

        typesImports =
            List.map (\( moduleName, typeSet ) -> ImportStatement moduleName Nothing (toExport typeSet)) <|
                Dict.toList
                    importsDict
    in
        List.map printStatement (extImports2 ++ typesImports)


toExportHelper typesDict name =
    TypeExport (TypeName.toSingleName name) (getExportSet typesDict name)


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
