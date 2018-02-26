module ParserExtensions exposing (..)

import Ast.Statement exposing (..)
import StatementFilters exposing (extractMetaComment, extractType, asFilter)
import Model exposing (MetaComment(..))


applyMetaComments : List Statement -> List Statement
applyMetaComments stmnts =
    let
        foldResult =
            List.foldl foldHelper (FoldHelper Nothing []) stmnts
    in
        List.reverse foldResult.statements


type alias FoldHelper =
    { metaComment : Maybe MetaComment
    , statements : List Statement
    }


foldHelper : Statement -> FoldHelper -> FoldHelper
foldHelper item accum =
    let
        f1 =
            { accum | metaComment = Nothing }

        f2 =
            { accum | metaComment = Nothing, statements = item :: accum.statements }

        metaComment =
            extractMetaComment item
    in
        if asFilter <| metaComment then
            { accum | metaComment = metaComment }
        else if asFilter <| extractType item then
            case accum.metaComment of
                Just Ignore ->
                    f1

                _ ->
                    f2
        else
            f2
