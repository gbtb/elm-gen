module ParserExtensions exposing (..)

import Ast.Statement exposing (..)
import StatementFilters exposing (extractMetaComment, extractType, asFilter)


applyMetaComments : List Statement -> List Statement
applyMetaComments stmnts =
    let
        foldResult =
            List.foldl foldHelper (FoldHelper False []) stmnts
    in
        List.reverse foldResult.statements


type alias FoldHelper =
    { hadMetaComment : Bool
    , statements : List Statement
    }


foldHelper : Statement -> FoldHelper -> FoldHelper
foldHelper item accum =
    let
        f1 =
            { accum | hadMetaComment = False }

        f2 =
            { accum | hadMetaComment = False, statements = item :: accum.statements }
    in
        if asFilter <| extractMetaComment item then
            { accum | hadMetaComment = True }
        else if asFilter <| extractType item then
            if accum.hadMetaComment then
                f1
            else
                f2
        else
            f2
