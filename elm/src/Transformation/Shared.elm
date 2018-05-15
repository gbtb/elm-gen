module Transformation.Shared exposing (..)

import Set
import Dict
import Model exposing (TypeName)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import TypeName exposing (..)
import ReadConfig exposing (..)


type alias TransformationContext =
    { decoderPrefix : String
    , assumeUnionTypeDefaultConstructor : Bool
    , knownTypes : Dict.Dict TypeName (List String)
    , makeName : String -> String
    , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
    , defaultUnionValues : Dict.Dict TypeName Expression
    , dontDeclareTypes : Set.Set TypeName
    }


defaultContext isDecoders =
    initContext isDecoders "JD" defaultDecoderNameFunc Dict.empty Dict.empty Set.empty


qualifiedName prefix name =
    if String.length prefix > 0 then
        [ prefix, name ]
    else
        [ name ]


variable prefix x =
    Variable <| qualifiedName prefix x


knownTypesForDecoders prefix =
    [ "Int", "Float", "String", "List", "Array", "Char", "Bool" ]
        |> List.map (\type_ -> ( TypeName.fromStr type_, qualifiedName prefix (String.toLower type_) ))
        |> Dict.fromList


knownTypesForEncoders prefix nameFunc =
    ([ "Int", "Float", "String", "Char", "Bool" ]
        |> List.map (\type_ -> ( TypeName.fromStr type_, qualifiedName prefix (String.toLower type_) ))
    )
        ++ ([ "List", "Array" ] |> List.map (\type_ -> ( TypeName.fromStr type_, qualifiedName "" (TypeName.getDecoderName [ type_ ] nameFunc) )))
        |> Dict.fromList


initContext isDecoders prefix nameFunc userDefinedTypes d1 d2 s =
    { decoderPrefix = prefix
    , assumeUnionTypeDefaultConstructor = False
    , knownTypes =
        (if isDecoders then
            knownTypesForDecoders prefix
         else
            knownTypesForEncoders prefix nameFunc
        )
            |> Dict.union userDefinedTypes
    , makeName = nameFunc
    , defaultRecordValues = d1
    , defaultUnionValues = d2
    , dontDeclareTypes = s
    }
