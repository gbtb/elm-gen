module StatementFilters exposing (..)

import Ast.Statement exposing (..)


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
            Just consName

        TypeDeclaration (TypeConstructor [ consName ] []) _ ->
            Just consName

        _ ->
            Nothing


extractModuleDeclaration s =
    case s of
        ModuleDeclaration m _ ->
            Just m

        _ ->
            Nothing


extractImport s =
    case s of
        ImportStatement moduleName _ mbExportSet ->
            Just ( moduleName, mbExportSet )

        _ ->
            Nothing


extractDecoder s =
    case s of
        FunctionTypeDeclaration name type_ ->
            case type_ of
                TypeConstructor [ "JD", "Decoder" ] [ TypeConstructor [ typeName ] [] ] ->
                    Just typeName

                _ ->
                    Nothing

        _ ->
            Nothing


extractDecoderName s =
    case s of
        FunctionTypeDeclaration name type_ ->
            case type_ of
                TypeConstructor [ "JD", "Decoder" ] [ TypeConstructor [ typeName ] [] ] ->
                    Just name

                _ ->
                    Nothing

        _ ->
            Nothing


extractMetaComment s =
    case s of
        Comment " #Ignore" ->
            Just " #Ignore"

        _ ->
            Nothing
