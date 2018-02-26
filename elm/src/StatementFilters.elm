module StatementFilters exposing (..)

import Ast.Statement exposing (..)
import Regex
import Model exposing (MetaComment(..))


asFilter : Maybe a -> Bool
asFilter mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False


extractType s =
    case s of
        TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
            Just ( consName, False )

        TypeDeclaration (TypeConstructor [ consName ] []) _ ->
            Just ( consName, True )

        _ ->
            Nothing


extractModuleDeclaration s =
    case s of
        ModuleDeclaration m _ ->
            Just m

        PortModuleDeclaration m _ ->
            Just m

        _ ->
            Nothing


extractImport s =
    case s of
        ImportStatement moduleName _ mbExportSet ->
            Just ( moduleName, mbExportSet )

        _ ->
            Nothing


extractFunctionTypeDecl s =
    case s of
        FunctionTypeDeclaration name stmnt ->
            Just name

        _ ->
            Nothing


extractDecoder tcName s =
    case s of
        FunctionTypeDeclaration name type_ ->
            case type_ of
                TypeConstructor tcName [ TypeConstructor [ typeName ] [] ] ->
                    Just ( typeName, name )

                _ ->
                    Nothing

        _ ->
            Nothing


extractDecoderName tcName s =
    case s of
        FunctionTypeDeclaration name type_ ->
            case type_ of
                TypeConstructor tcName [ TypeConstructor [ typeName ] [] ] ->
                    Just name

                _ ->
                    Nothing

        _ ->
            Nothing


extractEncoder tcName s =
    case s of
        FunctionTypeDeclaration name type_ ->
            eeHelper tcName type_ |> Maybe.map (\typeName -> ( typeName, name ))

        _ ->
            Nothing


eeHelper tcName s =
    case s of
        TypeApplication (TypeConstructor [ typeName ] _) (TypeConstructor tcName []) ->
            Just typeName

        TypeApplication s1 s2 ->
            eeHelper tcName s2

        _ ->
            Nothing


extractMetaComment s =
    case s of
        Comment str ->
            if Regex.contains (Regex.regex "//Ignore") str then
                Just Ignore
            else if Regex.contains (Regex.regex "//DefaultValue") str then
                Just DefaultValue
            else
                Nothing

        _ ->
            Nothing
