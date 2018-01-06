port module Main exposing (main)

{-| This is main module for all the parsing and transforming work for decoders to make.

#Main func
@docs main
-}

--required to interop with js

import Json.Decode


--

import Platform exposing (program)
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder)
import Printer exposing (printStatement)
import List.Extra as List
import Composer exposing (generate, composeFile, resolveDependencies, makeFileLoadRequest, importsFilter)
import PrintRepr exposing (produceString, (+>), PrintRepr(..))
import Utils exposing (..)
import Set
import Dict
import Task
import Model exposing (..)


type Msg
    = Parse ( String, List String )
    | ResolveDependencies
    | Generate
    | Print


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ( fileContents, fileNames ) ->
            let
                parsedStatements =
                    parseModule operators fileContents
            in
                if List.length model.parsedStatements > 0 then
                    updateAdditionalParse model parsedStatements fileNames
                else
                    updateInitialParse model parsedStatements

        ResolveDependencies ->
            let
                new_model =
                    resolveDependencies model

                imports =
                    List.filter importsFilter model.parsedStatements
            in
                if Set.isEmpty new_model.unknownTypes then
                    ( new_model, Cmd.batch [ logMessage "Parsing is complete, all required types are loaded...", makeCmd Generate ] )
                else
                    let
                        importsDict =
                            makeFileLoadRequest new_model
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
                ( model, Cmd.batch [ logMessage "Printing...", output fileContent ] )


updateInitialParse model parsedStatements =
    ( { model
        | parsedStatements =
            case parsedStatements of
                Err _ ->
                    Debug.crash "Failed to parse module!"

                Ok ( _, _, statements ) ->
                    statements
      }
    , Cmd.batch [ logMessage "Parsing files...", makeCmd ResolveDependencies ]
    )


updateAdditionalParse model parsedStatements fileNames =
    ( { model
        | newlyParsedStatements =
            case parsedStatements of
                Err _ ->
                    Debug.crash "Failed to parse module!"

                Ok ( _, _, statements ) ->
                    statements
      }
    , Cmd.batch [ logMessage <| "Parsing additional files: " ++ String.join ", " fileNames, makeCmd ResolveDependencies ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    input Parse


{-| Main func as it is
-}
main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port output : String -> Cmd msg


port requestFiles : List (List String) -> Cmd msg


port logMessage : String -> Cmd msg


port errorMessage : String -> Cmd msg


port input : (( String, List String ) -> msg) -> Sub msg
