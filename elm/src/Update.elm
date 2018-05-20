module Update exposing (update, Msg(..), PseudoCmd(..))

import ReadConfig exposing (readConfig)
import Model exposing (..)
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Composer exposing (generate, composeFile, resolveDependencies, makeFileLoadRequest)
import StatementFilters exposing (extractImport, asFilter)
import ParserExtensions exposing (applyMetaComments)
import Utils exposing (..)
import Set
import Dict
import Model exposing (..)


type Msg
    = Parse InputInfo
    | ResolveDependencies
    | Generate
    | Print
    | ReadConfig String
    | PseudoCmd PseudoCmd


type PseudoCmd
    = LogMessage String
    | ErrorMessage String
    | Output ( String, String )
    | RequestFiles (List (List String))


update comms msg model =
    case msg of
        ReadConfig str ->
            case readConfig str of
                Ok conf ->
                    { model | config = conf } ! []

                Err e ->
                    model ! [ comms.errorMessage <| "Can't decode provided config file. Elm error was:\n" ++ e ]

        Parse inputInfo ->
            let
                parsedStatements =
                    parseModule operators inputInfo.fileContents
            in
                if List.length model.parsedStatements > 0 then
                    updateAdditionalParse comms
                        model
                        parsedStatements
                        inputInfo.fileNames
                        inputInfo.genCommand
                else
                    updateInitialParse comms model parsedStatements inputInfo.fileNames inputInfo.rootDir inputInfo.genCommand

        ResolveDependencies ->
            let
                new_model =
                    resolveDependencies model

                imports =
                    List.filter (extractImport >> asFilter) model.parsedStatements
            in
                case new_model of
                    Ok new_model ->
                        if Set.isEmpty new_model.unknownTypes then
                            ( new_model, Cmd.batch [ comms.logMessage "Parsing is complete, all required types are loaded...", makeCmd Generate ] )
                        else
                            let
                                importsDict =
                                    makeFileLoadRequest new_model
                            in
                                case importsDict of
                                    Ok dict ->
                                        ( { new_model | importsDict = Dict.union new_model.importsDict dict }
                                        , comms.requestFiles <| Dict.keys dict
                                        )

                                    Err e ->
                                        ( new_model, comms.errorMessage e )

                    Err e ->
                        ( model, comms.errorMessage e )

        Generate ->
            let
                generationRes =
                    generate model
            in
                case generationRes of
                    Ok modelAfterGeneration ->
                        ( modelAfterGeneration
                        , Cmd.batch [ comms.logMessage "Generating decoders...", makeCmd Print ]
                        )

                    Err e ->
                        ( model, comms.errorMessage e )

        Print ->
            let
                fileContent =
                    composeFile model
            in
                case fileContent of
                    Ok fileContent ->
                        ( model, Cmd.batch [ comms.logMessage "Printing...", comms.output ( model.outputFileName, fileContent ) ] )

                    Err e ->
                        ( model, comms.errorMessage <| "Error during printing stage: " ++ e )

        PseudoCmd _ ->
            ( model, Cmd.none )


updateInitialParse comms model parsedStatements fileNames rootDir genCommand =
    let
        metaParseResult =
            applyMetaComments parsedStatements_

        parsedStatements_ =
            case parsedStatements of
                Err _ ->
                    Debug.crash "Failed to parse module!"

                Ok ( _, _, statements ) ->
                    statements

        updatedConfig =
            ReadConfig.updateConfig model.config genCommand
    in
        ( { model
            | parsedStatements = metaParseResult.statements
            , genCommand = genCommand
            , outputFileName = ReadConfig.makeOutputFileName updatedConfig (List.head fileNames |> fromJust "Output file name was not provided!")
            , rootDir = rootDir
            , defaultRecordValues = Dict.union model.defaultRecordValues metaParseResult.defaultRecordValues
            , defaultUnionValues =
                Dict.union model.defaultUnionValues metaParseResult.defaultUnionValues
            , dontDeclareTypes = Set.union model.dontDeclareTypes metaParseResult.dontDeclareTypes
            , config = updatedConfig
          }
        , Cmd.batch [ comms.logMessage "Parsing files...", makeCmd ResolveDependencies ]
        )


updateAdditionalParse comms model parsedStatements fileNames genCommand =
    let
        metaParseResult =
            applyMetaComments parsedStatements_

        parsedStatements_ =
            case parsedStatements of
                Err _ ->
                    Debug.crash "Failed to parse module!"

                Ok ( _, _, statements ) ->
                    statements
    in
        ( { model
            | newlyParsedStatements = metaParseResult.statements
            , defaultRecordValues = Dict.union model.defaultRecordValues metaParseResult.defaultRecordValues
            , defaultUnionValues = Dict.union model.defaultUnionValues metaParseResult.defaultUnionValues
            , dontDeclareTypes = Set.union model.dontDeclareTypes metaParseResult.dontDeclareTypes
          }
        , Cmd.batch [ comms.logMessage <| "Parsing additional files: " ++ String.join ", " fileNames, makeCmd ResolveDependencies ]
        )
