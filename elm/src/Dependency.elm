module Dependency exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Dict
import Set
import Utils exposing (..)


{-
   makeDependencyGraph : List Statement -> Dict.Dict String (Set.Set String)
   makeDependencyGraph statements =
       List.foldl () Dict.empty statements


   traversal st graph =
       case st of
           TypeDeclaration typename typeDef ->
               let
                   name =
                       getTypeName typename
               in
                   Dict.update name (updateDependencies <| getDependencies Set.empty typeDef) graph

           TypeAliasDeclaration typename typeDef ->
               let
                   name =
                       getTypeName typename
               in
                   Dict.update name (updateDependencies <| getDependencies Set.empty typeDef) graph

           _ ->
               graph
-}


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



{- | TypeVariable Name
   | TypeRecordConstructor Type (List (Name, Type))
   | TypeRecord (List (Name, Type))
   | TypeTuple (List Type)
   | TypeApplication Type Type
-}


updateDependencies newValues oldValues =
    case oldValues of
        Nothing ->
            newValues

        Just oldValues ->
            Set.union oldValues newValues
