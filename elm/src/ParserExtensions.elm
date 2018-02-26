module ParserExtensions exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import StatementFilters exposing (extractMetaComment, extractType, asFilter, extractRecordTypeDefault, extractUnionTypeDefault)
import Model exposing (MetaComment(..))
import Maybe.Extra as Maybe
import Dict


applyMetaComments :
    List Statement
    -> { statements : List Statement
       , defaultRecordValues : Dict.Dict ( String, String ) Expression
       , defaultUnionValues : Dict.Dict String Expression
       }
applyMetaComments stmnts =
    let
        foldResult =
            List.foldl foldHelper initFoldHelper stmnts
    in
        { statements = List.reverse foldResult.statements
        , defaultRecordValues = foldResult.defaultRecordValues
        , defaultUnionValues = foldResult.defaultUnionValues
        }


type alias FoldHelper =
    { metaComment : Maybe MetaComment
    , typeName : Maybe String
    , defaultRecordValues : Dict.Dict ( String, String ) Expression
    , defaultUnionValues : Dict.Dict String Expression
    , statements : List Statement
    }


initFoldHelper =
    { metaComment = Nothing
    , typeName = Nothing
    , defaultRecordValues = Dict.empty
    , defaultUnionValues = Dict.empty
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
                    if asFilter <| extractType item && meta == Ignore then
                        f1
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
