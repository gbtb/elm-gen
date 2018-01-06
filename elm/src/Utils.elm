module Utils exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Task
import Char


fromJust err m =
    case m of
        Just x ->
            x

        Nothing ->
            Debug.crash err


getTypeName t =
    case t of
        TypeConstructor [ name ] [] ->
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


getDecoderName typeName =
    case String.uncons typeName of
        Just ( h, tail ) ->
            String.cons (Char.toLower h) (tail ++ "Decoder")

        Nothing ->
            ""
