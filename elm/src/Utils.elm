module Utils exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Task
import Char
import Set
import Dict
import Model exposing (GenCommand(..))


fromJust err m =
    case m of
        Just x ->
            x

        Nothing ->
            Debug.crash err


fromOk r =
    case r of
        Ok x ->
            x

        Err e ->
            Debug.crash (toString e)


getTypeName t =
    case t of
        TypeConstructor name [] ->
            name

        _ ->
            Debug.crash "Cannot extract typeName!"


getTypeNameFromStatement st =
    case st of
        TypeAliasDeclaration typeName _ ->
            getTypeName typeName

        TypeDeclaration typeName _ ->
            getTypeName typeName

        _ ->
            Debug.crash "Cannot extract typeName"


makeCmd =
    Task.perform identity << Task.succeed


keysSet d =
    Set.fromList <| Dict.keys d


willGenDecoder genCommand =
    genCommand == Decoders || genCommand == DecodersAndEncoders


willGenEncoder genCommand =
    genCommand == Encoders || genCommand == DecodersAndEncoders


values l =
    List.foldl
        (\item accum ->
            case item of
                Ok v ->
                    accum ++ [ v ]

                Err _ ->
                    accum
        )
        []
        l


flattenResult r =
    case r of
        Ok (Ok a) ->
            Ok a

        Ok (Err e) ->
            Err e

        Err e ->
            Err e
