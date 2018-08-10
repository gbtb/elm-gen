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
       , fieldNameConversions : Dict.Dict String (Dict.Dict String String)
       , fieldNameConversionApplications : Dict.Dict TypeName String
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
        , fieldNameConversions = foldResult.fieldNameConversions
        , fieldNameConversionApplications = foldResult.fieldNameConversionApplications
        , defaultUnionValues = foldResult.defaultUnionValues
        , dontDeclareTypes = foldResult.dontDeclareTypes
        }


type alias FoldHelper =
    { metaComment : Maybe MetaComment
    , typeName : Maybe TypeName
    , defaultRecordValues : Dict.Dict ( TypeName, String ) Expression
    , fieldNameConversions : Dict.Dict String (Dict.Dict String String)
    , fieldNameConversionApplications : Dict.Dict TypeName String
    , defaultUnionValues : Dict.Dict TypeName Expression
    , dontDeclareTypes : Set.Set TypeName
    , statements : List Statement
    }


initFoldHelper =
    { metaComment = Nothing
    , typeName = Nothing
    , defaultRecordValues = Dict.empty
    , fieldNameConversions = Dict.empty
    , fieldNameConversionApplications = Dict.empty
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
                    else if meta == NoDeclaration then
                        case extractType item of
                            Just ( typeName, _ ) ->
                                { f2 | dontDeclareTypes = Set.insert typeName f2.dontDeclareTypes }

                            Nothing ->
                                f2
                    else if meta == DefaultValue then
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
                    else
                        f3

                Nothing ->
                    f2


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
