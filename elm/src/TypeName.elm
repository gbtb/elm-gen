module TypeName exposing (..)

import Model exposing (TypeName)
import Char


fromStr : String -> TypeName
fromStr str =
    [ str ]


toStr : TypeName -> String
toStr typename =
    String.join "." typename


toSingleName : TypeName -> String
toSingleName typeName =
    case typeName of
        [ a ] ->
            a

        _ ->
            Debug.crash "I expect single (not module-prefixed) type name!"


toLowerCaseName : TypeName -> String
toLowerCaseName l =
    let
        lc typeName =
            case String.uncons typeName of
                Just ( h, tail ) ->
                    String.cons (Char.toLower h) (tail)

                Nothing ->
                    ""
    in
        case l of
            [] ->
                ""

            [ a ] ->
                lc a

            a :: cons ->
                lc a ++ (String.join "" cons)


getDecoderName typeName makeName =
    makeName <| toLowerCaseName typeName


getNamespace typeName =
    case typeName of
        [] ->
            ""

        [ a ] ->
            ""

        h :: cons ->
            h
