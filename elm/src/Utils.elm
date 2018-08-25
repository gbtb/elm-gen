module Utils exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Task
import Char
import Set
import Dict
import Model exposing (GenCommand(..))


getTypeName t =
    case t of
        TypeConstructor name [] ->
            Ok name

        _ ->
            Err "Cannot extract typeName!"


getTypeNameFromStatement st =
    case st of
        TypeAliasDeclaration typeName _ ->
            getTypeName typeName

        TypeDeclaration typeName _ ->
            getTypeName typeName

        _ ->
            Err "Cannot extract typeName"


makeCmd =
    Task.perform identity << Task.succeed


keysSet d =
    Set.fromList <| Dict.keys d


{-| Checks if this GenCommand result in generating decoder
-}
willGenDecoder genCommand =
    genCommand == Decoders || genCommand == DecodersAndEncoders


{-| Checks if this GenCommand result in generating encoder
-}
willGenEncoder genCommand =
    genCommand == Encoders || genCommand == DecodersAndEncoders


values : List (Result a b) -> List b
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
