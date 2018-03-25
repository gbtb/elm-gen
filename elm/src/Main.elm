port module Main exposing (main)

{-| This is main module for all the parsing and transforming work for decoders to make.

#Main func
@docs main
-}

--required to interop with js

import Json.Decode as JD
import Json.Encode as JE


--

import Platform exposing (program)
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Composer exposing (generate, composeFile, resolveDependencies, makeFileLoadRequest)
import StatementFilters exposing (extractImport, asFilter)
import ParserExtensions exposing (applyMetaComments)
import Utils exposing (..)
import Set
import Dict
import Model exposing (..)
import ModelDecoders exposing (..)
import ReadConfig exposing (readConfig)


type Msg
    = Parse InputInfo
    | ResolveDependencies
    | Generate
    | Print
    | ReadConfig String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadConfig str ->
            case readConfig str of
                Ok conf ->
                    { model | config = conf } ! []

                Err e ->
                    model ! [ errorMessage <| "Can't decode provided config file. Elm error was:\n" ++ e ]

        Parse inputInfo ->
            let
                parsedStatements =
                    parseModule operators inputInfo.fileContents
            in
                if List.length model.parsedStatements > 0 then
                    updateAdditionalParse model
                        parsedStatements
                        inputInfo.fileNames
                        inputInfo.genCommand
                else
                    updateInitialParse model parsedStatements inputInfo.fileNames inputInfo.rootDir inputInfo.genCommand

        ResolveDependencies ->
            let
                new_model =
                    resolveDependencies model

                imports =
                    List.filter (extractImport >> asFilter) model.parsedStatements
            in
                if Set.isEmpty new_model.unknownTypes then
                    ( new_model, Cmd.batch [ logMessage "Parsing is complete, all required types are loaded...", makeCmd Generate ] )
                else
                    let
                        importsDict =
                            makeFileLoadRequest (Debug.log "n" new_model)
                    in
                        case importsDict of
                            Ok dict ->
                                ( { new_model | importsDict = Dict.union new_model.importsDict dict }
                                , requestFiles <| Dict.keys dict
                                )

                            Err e ->
                                ( new_model, errorMessage e )

        Generate ->
            ( generate model
            , Cmd.batch [ logMessage "Generating decoders...", makeCmd Print ]
            )

        Print ->
            let
                fileContent =
                    composeFile model
            in
                case fileContent of
                    Ok fileContent ->
                        ( model, Cmd.batch [ logMessage "Printing...", output ( model.outputFileName, fileContent ) ] )

                    Err e ->
                        ( model, errorMessage <| "Error during printing stage: " ++ e )


updateInitialParse model parsedStatements fileNames rootDir genCommand =
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
            , config = updatedConfig
          }
        , Cmd.batch [ logMessage "Parsing files...", makeCmd ResolveDependencies ]
        )


updateAdditionalParse model parsedStatements fileNames genCommand =
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
          }
        , Cmd.batch [ logMessage <| "Parsing additional files: " ++ String.join ", " fileNames, makeCmd ResolveDependencies ]
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ input <| (\value -> JD.decodeValue inputInfoDecoder value |> fromOk |> Parse)
        , config <| ReadConfig
        ]


{-| Main func as it is
-}
main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port config : (String -> msg) -> Sub msg


port output : ( String, String ) -> Cmd msg


port requestFiles : List (List String) -> Cmd msg


port logMessage : String -> Cmd msg


port errorMessage : String -> Cmd msg


port input : (JE.Value -> msg) -> Sub msg
