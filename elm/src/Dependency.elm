module Dependency exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import Utils exposing (..)


knownTypes : Set.Set String
knownTypes =
    Set.fromList [ "Bool", "Int", "Float", "String", "List", "Array" ]


makeDependencyGraph : Set.Set String -> List Statement -> Dict.Dict String (Set.Set String)
makeDependencyGraph knownTypes statements =
    List.foldl (graphHelper knownTypes) Dict.empty statements


graphHelper : Set.Set String -> Statement -> Dict.Dict String (Set.Set String) -> Dict.Dict String (Set.Set String)
graphHelper knownTypes stmt =
    case stmt of
        TypeDeclaration typeName _ ->
            let
                name =
                    getTypeName typeName
            in
                Dict.update name (updateDependencies <| getDependencies knownTypes stmt)

        TypeAliasDeclaration typeName _ ->
            let
                name =
                    getTypeName typeName
            in
                Dict.update name (updateDependencies <| getDependencies knownTypes stmt)

        _ ->
            Debug.crash "Unsupported type"


setdiff =
    flip Set.diff


getDependencies : Set.Set String -> Statement -> Set.Set String
getDependencies knownTypes type_ =
    case type_ of
        TypeDeclaration _ listOfConstructors ->
            setdiff knownTypes <|
                List.foldl (\item accum -> traverseType knownTypes item False |> Set.union accum) (Set.empty) listOfConstructors

        TypeAliasDeclaration _ recordType ->
            setdiff knownTypes <| traverseType knownTypes recordType False

        _ ->
            Debug.crash "Unsupported type"


traverseType knownTypes type_ useQualType =
    let
        helper startSet listOfTypes =
            setdiff knownTypes <|
                List.foldl
                    (\item accum -> traverseType knownTypes item True |> Set.union accum)
                    startSet
                    listOfTypes
    in
        case type_ of
            TypeConstructor qualType listOfTypes ->
                let
                    start =
                        if useQualType then
                            (Set.fromList [ String.join "." qualType ])
                        else
                            Set.empty
                in
                    helper start listOfTypes

            TypeTuple listOfTypes ->
                helper Set.empty listOfTypes

            TypeRecord listOfFields ->
                List.map Tuple.second listOfFields |> helper Set.empty

            _ ->
                Debug.crash "Unsupported type"


updateDependencies newValues oldValues =
    case oldValues of
        Nothing ->
            if Set.size newValues > 0 then
                Just newValues
            else
                Nothing

        Just oldValues ->
            Just <| Set.union oldValues newValues
