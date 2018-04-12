module Dependency exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import Utils exposing (..)
import TypeName
import Model exposing (TypeSet)


knownTypes : TypeSet
knownTypes =
    Set.map TypeName.fromStr <| Set.fromList [ "Bool", "Char", "Int", "Float", "String" ]



--makeDependencyGraph : Set.Set String -> Set.Set String -> List Statement -> ( Set.Set String, Dict.Dict String (Set.Set String) )


makeDependencyGraph nonHeads knownTypes statements =
    let
        ( nonHeads_, graph ) =
            List.foldl (graphHelper knownTypes) ( nonHeads, Dict.empty ) statements
    in
        ( Set.diff (Set.fromList <| Dict.keys graph) nonHeads_, graph )



--graphHelper : Set.Set String -> Statement -> ( Set.Set String, Dict.Dict String (Set.Set String) ) -> ( Set.Set String, Dict.Dict String (Set.Set String) )


graphHelper knownTypes stmt ( nonHeads, graph ) =
    let
        retrievedDeps =
            getDependencies knownTypes (Debug.log "deps" stmt)
    in
        case stmt of
            TypeDeclaration typeName _ ->
                let
                    name =
                        getTypeName typeName
                in
                    ( Set.union nonHeads retrievedDeps
                    , Dict.update name (updateDependencies retrievedDeps) graph
                        |> (updateGraphForHardcodedTypes retrievedDeps)
                    )

            TypeAliasDeclaration typeName _ ->
                let
                    name =
                        getTypeName typeName
                in
                    ( Set.union nonHeads retrievedDeps
                    , Dict.update name (updateDependencies retrievedDeps) graph
                        |> (updateGraphForHardcodedTypes retrievedDeps)
                    )

            _ ->
                Debug.crash "Unsupported type"


updateGraphForHardcodedTypes retrievedDeps d =
    let
        hard =
            Set.intersect (Set.map TypeName.fromStr <| Set.fromList [ "Maybe", "List", "Array" ]) retrievedDeps
    in
        Set.foldl (\item dict -> Dict.insert item Set.empty dict) d hard


setdiff =
    flip Set.diff


getDependencies : TypeSet -> Statement -> TypeSet
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
                            (Set.fromList [ qualType ])
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
            Just newValues

        Just oldValues ->
            Just <| Set.union oldValues newValues
