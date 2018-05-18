module Imports exposing (printImports, importFoldHelper, getTypes)

import Set
import Ast.Statement exposing (..)
import Dict
import List.Extra as List
import Model exposing (TypeName)
import TypeName
import Utils exposing (..)
import Printer exposing (printStatement)
import StatementFilters exposing (..)
import Maybe.Extra as Maybe
import Config exposing (..)


importFoldHelper importStmt ( unknownTypes, modulesDict ) =
    let
        defaultReturn =
            Ok ( unknownTypes, modulesDict )
    in
        if Set.isEmpty unknownTypes then
            defaultReturn
        else
            case importStmt of
                ImportStatement moduleName_ alias_ mbExportSet ->
                    let
                        moduleName =
                            Maybe.withDefault moduleName_ (Maybe.map List.singleton alias_)
                    in
                        case mbExportSet of
                            Just exportSet ->
                                filterOutConcreteTypesImports unknownTypes exportSet moduleName modulesDict
                                    |> filterOutGeneralImports moduleName
                                    |> Ok

                            Nothing ->
                                filterOutGeneralImports moduleName ( unknownTypes, modulesDict ) |> Ok

                _ ->
                    Err ("Non-import statement passed to function: " ++ toString importStmt)


filterOutConcreteTypesImports unknownTypes exportSet moduleName modulesDict =
    let
        importedTypes =
            Set.fromList <| getImportedTypes exportSet
    in
        ( Set.diff unknownTypes importedTypes
        , Dict.insert moduleName (Set.intersect unknownTypes importedTypes) modulesDict
        )


filterOutGeneralImports moduleName ( unknownTypes, modulesDict ) =
    let
        ( importedTypes, unknownTypes_ ) =
            Set.partition (\t -> List.isPrefixOf moduleName t) unknownTypes
    in
        if Set.size importedTypes == 0 then
            ( unknownTypes, modulesDict )
        else
            ( unknownTypes_, Dict.update moduleName (updater importedTypes) modulesDict )


updater new contained =
    case contained of
        Nothing ->
            Just new

        Just c ->
            Just <| Set.union c new


getImportedTypes : ExportSet -> List TypeName
getImportedTypes exportSet =
    case exportSet of
        SubsetExport listOfExports ->
            List.foldl (\exp list -> list ++ getImportedTypes exp) [] listOfExports

        TypeExport typeName _ ->
            [ TypeName.fromStr typeName ]

        _ ->
            []


printImports command importsDict typesDict conf =
    let
        encodersImports =
            [ ImportStatement [ "Json", "Encode" ]
                (case conf.encode of
                    DontTouch ->
                        Nothing

                    Replace str ->
                        Just str
                )
                (Nothing)
            ]

        decodeAlias =
            case conf.decode of
                DontTouch ->
                    Nothing

                Replace str ->
                    Just str

        decodersImports =
            [ ImportStatement [ "Json", "Decode" ] (decodeAlias) (Nothing)
            , ImportStatement [ "Json", "Decode", "Pipeline" ] (decodeAlias) (Nothing)
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
                Dict.toList importsDict
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



--getTypes : GenCommand -> Dict.Dict TypeName String -> Dict.Dict TypeName String -> Set.Set TypeName -> List Statement -> List TypeName


{-|
Extracts types from type declarations
-}
getTypes genCommand jsonModulesImports decoders encoders unknownTypes lst =
    let
        extractTypeFromDecoderOrEncoder st =
            extractDecoder [ getDecodePrefix jsonModulesImports.decode, "Decode" ] st
                |> Maybe.or (extractEncoder [ getEncodePrefix jsonModulesImports.decode, "Value" ] st)
                |> Maybe.map (\( typeName, _ ) -> ( typeName, False ))
    in
        List.sort <|
            List.concat <|
                List.filterMap
                    (\s ->
                        extractType s
                            |> Maybe.orElse (extractTypeFromDecoderOrEncoder s)
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
