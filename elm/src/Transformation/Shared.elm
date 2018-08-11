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
    , fieldNameMapping : Dict.Dict String (Dict.Dict String String)
    , fieldNameMappingApplications : Dict.Dict TypeName String
    }


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


initContext isDecoders prefix nameFunc userDefinedTypes d1 d2 s f1 f2 =
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
    , fieldNameMapping = f1
    , fieldNameMappingApplications = f2
    }


pipeOp =
    BinOp (Variable [ "|>" ])
