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
import Composer exposing (generate, composeFile)
import PrintRepr exposing (produceString, (+>), PrintRepr(..))
import Utils exposing (..)
import Set
import Dict
import Task
import Model exposing (..)


type Msg
    = Parse String
    | Generate
    | Print


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse string ->
            let
                parsedStatements =
                    parseModule operators string
            in
                ( { model
                    | parsedStatements =
                        case parsedStatements of
                            Err _ ->
                                Debug.crash "Failed to parse module!"

                            Ok ( _, _, statements ) ->
                                statements
                  }
                , Cmd.batch [ logMessage "Parsing files...", makeCmd Generate ]
                )

        Generate ->
            ( generate model
            , Cmd.batch [ logMessage "Parsing is complete, all required types are loaded...", logMessage "Generating decoders...", makeCmd Print ]
            )

        Print ->
            let
                fileContent =
                    composeFile model
            in
                ( model, Cmd.batch [ logMessage "Printing...", output fileContent ] )


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


port logMessage : String -> Cmd msg


port errorMessage : String -> Cmd msg


port input : (String -> msg) -> Sub msg
