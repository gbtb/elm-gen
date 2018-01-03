module Utils exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)


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
