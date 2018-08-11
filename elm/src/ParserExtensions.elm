module ParserExtensions exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import StatementFilters exposing (extractMetaComment, extractType, asFilter, extractRecordTypeDefault, extractUnionTypeDefault, extractRecord)
import Model exposing (MetaComment(..), TypeName)
import Maybe.Extra as Maybe
import Dict
import Set


applyMetaComments :
    List Statement
    -> { statements : List Statement
       , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
       , fieldNameMapping : Dict.Dict String (Dict.Dict String String)
       , fieldNameMappingApplications : Dict.Dict TypeName String
       , defaultUnionValues : Dict.Dict TypeName Expression
       , dontDeclareTypes : Set.Set TypeName
       }
applyMetaComments stmnts =
    let
        foldResult =
            List.foldl foldHelper initFoldHelper stmnts
    in
        { statements = List.reverse foldResult.statements
        , defaultRecordValues = foldResult.defaultRecordValues
        , fieldNameMapping = foldResult.fieldNameMapping
        , fieldNameMappingApplications = foldResult.fieldNameMappingApplications
        , defaultUnionValues = foldResult.defaultUnionValues
        , dontDeclareTypes = foldResult.dontDeclareTypes
        }


type alias FoldHelper =
    { metaComment : Maybe MetaComment
    , typeName : Maybe TypeName
    , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
    , fieldNameMapping : Dict.Dict String (Dict.Dict String String)
    , fieldNameMappingApplications : Dict.Dict TypeName String
    , defaultUnionValues : Dict.Dict TypeName Expression
    , dontDeclareTypes : Set.Set TypeName
    , statements : List Statement
    }


initFoldHelper =
    { metaComment = Nothing
    , typeName = Nothing
    , defaultRecordValues = Dict.empty
    , fieldNameMapping = Dict.empty
    , fieldNameMappingApplications = Dict.empty
    , defaultUnionValues = Dict.empty
    , dontDeclareTypes = Set.empty
    , statements = []
    }


foldHelper : Statement -> FoldHelper -> FoldHelper
foldHelper item accum =
    let
        f1 =
            { accum | metaComment = Nothing }

        f2 =
            { f1 | statements = item :: accum.statements }

        f3 =
            { f2 | typeName = Nothing }

        metaComment =
            extractMetaComment item
    in
        if asFilter <| metaComment then
            { accum | metaComment = metaComment }
        else
            case accum.metaComment of
                Just meta ->
                    if (asFilter <| extractType item) && meta == Ignore then
                        f1
                    else
                        metaCommentCaseHelper accum meta item f1 f2 f3

                Nothing ->
                    f2


metaCommentCaseHelper accum meta item f1 f2 f3 =
    case meta of
        NoDeclaration ->
            case extractType item of
                Just ( typeName, _ ) ->
                    { f2 | dontDeclareTypes = Set.insert typeName f2.dontDeclareTypes }

                Nothing ->
                    f2

        DefaultValue ->
            case accum.typeName of
                Nothing ->
                    case extractUnionTypeDefault item |> Maybe.orElse (extractRecordTypeDefault item) of
                        Just typeName ->
                            { accum | typeName = Just typeName }

                        Nothing ->
                            f3

                Just typeName ->
                    case extractDefaultValues accum item of
                        Just newAcc ->
                            newAcc

                        Nothing ->
                            f3

        FieldNameConversion ->
            case extractFieldNameConversion f1 item of
                Just newAcc ->
                    newAcc

                Nothing ->
                    f2

        FieldNameConversionApplication conversionName ->
            case extractType item of
                Just ( typeName, _ ) ->
                    { f2
                        | fieldNameMappingApplications =
                            Dict.insert typeName conversionName f2.fieldNameMappingApplications
                    }

                Nothing ->
                    accum

        _ ->
            f3


defaultValueHelper accum item =
    { accum
        | typeName =
            accum.metaComment
                |> Maybe.andThen
                    (\com ->
                        if com == DefaultValue then
                            extractUnionTypeDefault item
                                |> Maybe.orElse (extractRecordTypeDefault item)
                        else
                            Nothing
                    )
    }


extractDefaultValues accum item =
    Maybe.andThen
        (\typeName ->
            case item of
                FunctionDeclaration _ [] funcBody ->
                    extractRecord funcBody
                        |> Maybe.andThen (Just << extractRecordHelper accum typeName)
                        |> Maybe.orElse
                            (Just
                                ({ accum
                                    | defaultUnionValues = Dict.insert typeName funcBody accum.defaultUnionValues
                                    , typeName = Nothing
                                 }
                                )
                            )

                FunctionDeclaration _ [ _ ] funcBody ->
                    case funcBody of
                        RecordUpdate _ fieldList ->
                            Just <| extractRecordHelper accum typeName fieldList

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )
        accum.typeName


extractRecordHelper accum typeName fieldList =
    let
        resetTypeName a =
            { a | typeName = Nothing }
    in
        resetTypeName <|
            List.foldl
                (\item accum ->
                    { accum | defaultRecordValues = Dict.insert ( typeName, (Tuple.first item) ) (Tuple.second item) accum.defaultRecordValues }
                )
                accum
                fieldList


{-| This func tries to extract field name conversion
(aka special record mapping record field names to their aliases for decoding )
-}
extractFieldNameConversion accum item =
    case item of
        FunctionDeclaration funcName [] funcBody ->
            extractRecord funcBody
                |> Maybe.andThen
                    (\fields ->
                        List.foldl extractFieldNameConversionHelper (Just Dict.empty) fields
                    )
                |> Maybe.map
                    (\dict ->
                        { accum
                            | fieldNameMapping = Dict.insert funcName dict accum.fieldNameMapping
                        }
                    )

        _ ->
            Nothing


extractFieldNameConversionHelper ( name, expr ) accum =
    case accum of
        Nothing ->
            Nothing

        Just dict ->
            case expr of
                String s ->
                    Dict.insert name s dict |> Just

                _ ->
                    Nothing


resetTypeName a =
    { a | typeName = Nothing }
