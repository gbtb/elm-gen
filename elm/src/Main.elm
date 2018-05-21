port module Main exposing (main)

{-| This is main module for all the parsing and transforming work for decoders to make.

#Main func
@docs main
-}

--required to interop with js

import Json.Decode as JD
import Json.Encode as JE
import Ports exposing (..)


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
import Update exposing (update, Msg(..))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ input <|
            (\value ->
                case JD.decodeValue inputInfoDecoder value of
                    Ok info ->
                        Parse info

                    Err e ->
                        Debug.crash <| "Input info error! Probably TS-client & elm-server json protocol mismatch. Err: " ++ toString e
            )
        , config <| ReadConfig
        ]


{-| Main func as it is
-}
main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , update =
            update
                { logMessage = Ports.logMessage
                , errorMessage = Ports.errorMessage
                , output = output
                , requestFiles = requestFiles
                }
        , subscriptions = subscriptions
        }
