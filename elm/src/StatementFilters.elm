module StatementFilters exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Regex
import Model exposing (MetaComment(..), TypeName)
import TypeName
import Maybe.Extra as Maybe


asFilter : Maybe a -> Bool
asFilter mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False


extractType s =
    case s of
        TypeAliasDeclaration (TypeConstructor typeName []) (TypeRecord r) ->
            Just ( typeName, False )

        TypeAliasDeclaration (TypeConstructor typeName []) (TypeConstructor _ _) ->
            Just ( typeName, True )

        TypeDeclaration (TypeConstructor typeName []) _ ->
            Just ( typeName, True )

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


extractImportedModuleName s =
    case s of
        ImportStatement moduleName alias_ mbExportSet ->
            alias_ |> Maybe.map List.singleton |> Maybe.withDefault moduleName |> Just

        _ ->
            Nothing


extractFunctionTypeDecl s =
    case s of
        FunctionTypeDeclaration name stmnt ->
            Just name

        _ ->
            Nothing


extractDecoder : TypeName -> Statement -> Maybe ( TypeName, String )
extractDecoder tcName s =
    case s of
        FunctionTypeDeclaration name type_ ->
            case type_ of
                TypeConstructor tcName [ TypeConstructor l [] ] ->
                    Just ( l, name )

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
        TypeApplication (TypeConstructor typeName _) (TypeConstructor tcName []) ->
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
            else if Regex.contains (Regex.regex "//NoDeclaration") str then
                Just NoDeclaration
            else
                Nothing

        _ ->
            Nothing


extractUnionTypeDefault s =
    case s of
        FunctionTypeDeclaration _ (TypeConstructor name []) ->
            Just name

        _ ->
            Nothing


extractRecordTypeDefault s =
    case s of
        FunctionTypeDeclaration _ (TypeApplication (TypeConstructor name1 []) (TypeConstructor name2 [])) ->
            let
                name1_ =
                    name1

                name2_ =
                    name2
            in
                if name1_ == name2_ then
                    Just name1_
                else
                    Nothing

        _ ->
            Nothing


extractRecord s =
    case s of
        Record fieldList ->
            Just fieldList

        RecordUpdate _ fieldList ->
            Just fieldList

        _ ->
            Nothing
