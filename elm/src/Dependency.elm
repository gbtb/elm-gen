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
        res =
            List.foldl (graphHelper knownTypes) (Ok ( nonHeads, Dict.empty )) statements
    in
        Result.map (\( nonHeads_, graph ) -> ( Set.diff (Set.fromList <| Dict.keys graph) nonHeads_, graph )) res



--graphHelper : Set.Set String -> Statement -> ( Set.Set String, Dict.Dict String (Set.Set String) ) -> ( Set.Set String, Dict.Dict String (Set.Set String) )


graphHelper knownTypes stmt accum =
    let
        retrievedDepsRes =
            getDependencies knownTypes stmt
    in
        Result.andThen
            (\( nonHeads, graph ) ->
                case stmt of
                    TypeDeclaration typeName _ ->
                        let
                            nameRes =
                                getTypeName typeName
                        in
                            Result.map2
                                (\retrievedDeps name ->
                                    ( Set.union nonHeads retrievedDeps
                                    , Dict.update name (updateDependencies retrievedDeps) graph
                                        |> (updateGraphForHardcodedTypes retrievedDeps)
                                    )
                                )
                                retrievedDepsRes
                                nameRes

                    TypeAliasDeclaration typeName _ ->
                        let
                            nameRes =
                                getTypeName typeName
                        in
                            Result.map2
                                (\retrievedDeps name ->
                                    ( Set.union nonHeads retrievedDeps
                                    , Dict.update name (updateDependencies retrievedDeps) graph
                                        |> (updateGraphForHardcodedTypes retrievedDeps)
                                    )
                                )
                                retrievedDepsRes
                                nameRes

                    _ ->
                        Err ("Unsupported type: " ++ toString stmt)
            )
            accum


updateGraphForHardcodedTypes retrievedDeps d =
    let
        hard =
            Set.intersect (Set.map TypeName.fromStr <| Set.fromList [ "Maybe", "List", "Array" ]) retrievedDeps
    in
        Set.foldl (\item dict -> Dict.insert item Set.empty dict) d hard


setdiff =
    flip Set.diff


getDependencies : TypeSet -> Statement -> Result String TypeSet
getDependencies knownTypes type_ =
    case type_ of
        TypeDeclaration _ listOfConstructors ->
            Result.map
                (setdiff knownTypes)
            <|
                List.foldl (\item accumRes -> Result.map2 Set.union accumRes <| traverseType knownTypes item False) (Ok Set.empty) listOfConstructors

        TypeAliasDeclaration _ recordType ->
            Result.map (setdiff knownTypes) <| traverseType knownTypes recordType True

        _ ->
            Err <| "Unsupported type: " ++ toString type_


traverseType knownTypes type_ useQualType =
    let
        helper startSet listOfTypes =
            Result.map
                (\traversed ->
                    setdiff knownTypes traversed
                )
            <|
                List.foldl
                    (\item accumRes ->
                        Result.map2 Set.union
                            (traverseType knownTypes item True)
                            accumRes
                    )
                    (Ok startSet)
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
                case listOfTypes of
                    [ a ] ->
                        helper Set.empty listOfTypes

                    _ ->
                        helper (Set.fromList [ [ "Tuple" ++ (toString <| List.length listOfTypes) ] ]) listOfTypes

            TypeRecord listOfFields ->
                List.map Tuple.second listOfFields |> helper Set.empty

            _ ->
                Err "Unsupported type"


updateDependencies newValues oldValues =
    case oldValues of
        Nothing ->
            Just newValues

        Just oldValues ->
            Just <| Set.union oldValues newValues
