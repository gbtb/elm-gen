module Printer exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import List.Extra as List
import Result.Extra as Result
import Utils exposing (..)
import PrintRepr exposing (..)
import StatementFilters exposing (..)


type alias PrinterSettings =
    { tabs : Int
    , tabWidth : Int
    }


type alias PrintContext =
    { printedBinOp : Bool
    , flatList : Bool
    , nestedTypeApplication : Bool
    , nestedList : Bool
    }


initContext =
    { printedBinOp = False
    , flatList = False
    , nestedTypeApplication = False
    , nestedList = False
    }


printExpression : PrintContext -> Expression -> Result String PrintRepr
printExpression context e =
    let
        defaultContext =
            initContext
    in
        case e of
            Integer i ->
                defaultLine <| toString i

            String s ->
                defaultLine <| "\"" ++ s ++ "\""

            Variable l ->
                defaultLine <| String.join "." l

            Application func arg ->
                let
                    firstPart =
                        (printExpression context func)

                    concatOp =
                        case firstPart of
                            Ok (Lines _) ->
                                makeLines

                            Ok (Line _ _) ->
                                (+>)

                            _ ->
                                (+>)
                in
                    concatOp
                        firstPart
                        (if isSimpleExpression arg then
                            printExpression context arg
                         else
                            printExpression context arg
                                |> (if requireBraces arg then
                                        braces
                                    else
                                        identity
                                   )
                                |> Result.map (ident 1)
                        )

            BinOp op arg1 arg2 ->
                printBinOp context op arg1 arg2

            Access (Variable [ prefix ]) names ->
                defaultLine (String.join "." (prefix :: names))

            List exprList ->
                printList context exprList

            Record exprList ->
                printRecord context exprList

            Tuple exprList ->
                printTuple context exprList

            Case expr caseBranches ->
                makeLines
                    (defaultLine "case" +> (printExpression context expr) +> defaultLine "of")
                    (List.map (printCaseBranch context) caseBranches
                        |> Result.combine
                        |> Result.map (\l -> l |> List.intersperse (Line -1 "") |> Lines)
                    )

            _ ->
                Err ("Cant print this type of expression!: " ++ toString e)


printStatement : Statement -> Result String PrintRepr
printStatement stmt =
    case stmt of
        FunctionDeclaration name args body ->
            let
                firstLine =
                    prepend (defaultLine name) (printFunctionArgs args)
            in
                makeLines firstLine <| Result.map (ident 1) (printExpression initContext body)

        ModuleDeclaration moduleName exportSet ->
            printModuleDeclaration moduleName exportSet

        ImportStatement moduleName alias exportSet ->
            printImportStatement moduleName alias exportSet

        FunctionTypeDeclaration name type_ ->
            printFunctionTypeDeclaration name type_

        _ ->
            Err ("Print of this statement is unsupported(yet?): " ++ toString stmt)


printFunctionTypeDeclaration name type_ =
    defaultLine name :> defaultLine " : " :> printType initContext type_


printTypeFoldHelper : Result String PrintRepr -> Result String PrintRepr -> Result String PrintRepr
printTypeFoldHelper item accum =
    accum +> (defaultLine " ") +> item


printType : PrintContext -> Type -> Result String PrintRepr
printType ctx type_ =
    case type_ of
        TypeConstructor qualifiedType typesList ->
            defaultLine (String.join "." qualifiedType)
                :> (if List.length typesList > 0 then
                        defaultLine " "
                    else
                        defaultLine ""
                   )
                :> (List.map (printType { ctx | nestedTypeApplication = False }) typesList
                        |> PrintRepr.join " "
                   )

        TypeTuple typesList ->
            defaultLine "("
                :> (List.map (printType { ctx | nestedTypeApplication = False }) typesList
                        |> PrintRepr.join " "
                   )
                :> defaultLine ")"

        TypeApplication tc1 tc2 ->
            let
                printRepr =
                    printType { ctx | nestedTypeApplication = True } tc1
                        :> defaultLine " -> "
                        :> printType { ctx | nestedTypeApplication = False } tc2
            in
                if ctx.nestedTypeApplication then
                    defaultLine "(" :> printRepr :> defaultLine ")"
                else
                    printRepr

        TypeVariable name ->
            defaultLine name

        _ ->
            Err ("Cannot print this type(yet?): " ++ toString type_)


printBinOp context op arg1 arg2 =
    let
        rightPart op argContext arg2 =
            if isLeftAssoc op then
                prepend (printExpression initContext op) (printExpression argContext arg2)
            else
                (printExpression argContext arg2)

        leftPart op argContext arg1 =
            if isLeftAssoc op then
                (printExpression argContext arg1)
            else
                prepend (printExpression argContext arg1) (printExpression initContext op)

        lines =
            makeLines (leftPart op initContext arg1) <|
                if not context.printedBinOp then
                    Result.map (ident 1) <| rightPart op { context | printedBinOp = True } arg2
                else
                    rightPart op { context | printedBinOp = True } arg2
    in
        if (isSimpleExpression arg2) then
            prepend (leftPart op context arg1) (rightPart op initContext arg2) |> Result.orElseLazy (\_ -> lines)
        else
            lines


printList : PrintContext -> List Expression -> Result String PrintRepr
printList ctx exprList =
    let
        nestedList ctx =
            { ctx | nestedList = False }

        lines h cons =
            makeLines
                (List.foldl (\accum item -> makeLines item accum)
                    (prepend (defaultLine "[") <| printExpression (nestedList ctx) h)
                    (List.map (\expr -> printExpression (nestedList ctx) expr |> prepend (defaultLine ",")) cons)
                )
                (defaultLine "]")
    in
        case exprList of
            [] ->
                defaultLine "[]"

            [ a ] ->
                (defaultLine "[" +> (printExpression (nestedList ctx) a) +> defaultLine "]")
                    |> Result.orElseLazy
                        (\_ -> prepend (defaultLine "[") (makeLines (printExpression (nestedList ctx) a) <| defaultLine "]"))

            h :: cons ->
                if ctx.flatList {- && not ctx.nestedList -} then
                    (List.foldl (\accum item -> item :> accum)
                        (prepend (defaultLine "[") <| printExpression (nestedList ctx) h)
                        (List.map (\expr -> (defaultLine ",") +> printExpression (nestedList ctx) expr) cons)
                    )
                        +> (defaultLine "]")
                        |> Result.orElseLazy (\_ -> lines h cons)
                else
                    lines h cons


printTuple : PrintContext -> List Expression -> Result String PrintRepr
printTuple ctx exprList =
    let
        nestedList ctx =
            { ctx | nestedList = True }

        flatList ctx =
            { ctx | flatList = True }
    in
        case exprList of
            [] ->
                defaultLine "()"

            h :: cons ->
                (List.foldl (\accum item -> item :> accum)
                    (defaultLine "(" +> printExpression (nestedList ctx) h)
                    (List.map (\expr -> printExpression (nestedList <| flatList ctx) expr |> prepend (defaultLine ",")) cons)
                )
                    +> (defaultLine ")")
                    |> Result.orElseLazy
                        (\_ ->
                            makeLines
                                (List.foldl (\accum item -> item :> accum)
                                    (defaultLine "(" +> printExpression (nestedList ctx) h)
                                    (List.map (\expr -> printExpression (nestedList <| flatList ctx) expr |> prepend (defaultLine ",")) cons)
                                )
                                (defaultLine ")")
                        )


printCaseBranch ctx ( leftPart, rightPart ) =
    let
        l1 =
            Result.map (ident 1) <| printExpression ctx leftPart +> defaultLine "->"

        l2 =
            Result.map (ident 2) <| printExpression ctx rightPart
    in
        makeLines l1 l2


printImportStatement moduleName alias exportSet =
    let
        lineStart =
            defaultLine <| "import " ++ (String.join "." moduleName)
    in
        lineStart
            :> (alias |> Maybe.map (\a -> " as " ++ a) |> Maybe.withDefault "" |> defaultLine)
            :> (case exportSet of
                    Just AllExport ->
                        defaultLine " exposing (..)"

                    Just (SubsetExport typesList) ->
                        defaultLine " exposing ("
                            :> (List.map printExportSet typesList
                                    |> PrintRepr.join ", "
                               )
                            :> defaultLine ")"

                    Nothing ->
                        defaultLine ""

                    _ ->
                        Err "Export set print error"
               )


printRecord context exprList =
    case exprList of
        [] ->
            defaultLine "{}"

        [ ( name, expr ) ] ->
            defaultLine ("{ " ++ name ++ " =") +> printExpression context expr +> defaultLine "}"

        h :: cons ->
            makeLines
                (List.foldl
                    (\( name, expr ) accum -> makeLines accum <| defaultLine (", " ++ name ++ " =") +> printExpression context expr)
                    (defaultLine ("{ " ++ (Tuple.first h) ++ " =") +> printExpression context (Tuple.second h))
                    cons
                )
                (defaultLine "}")


printExportSet : ExportSet -> Result String PrintRepr
printExportSet es =
    case es of
        AllExport ->
            defaultLine ".."

        SubsetExport nestedList ->
            List.map printExportSet nestedList |> PrintRepr.join ", "

        FunctionExport name ->
            defaultLine name

        TypeExport name constructors ->
            defaultLine name
                :> case constructors of
                    Nothing ->
                        defaultLine ""

                    Just nestedEs ->
                        defaultLine "(" :> printExportSet nestedEs :> defaultLine ")"


printModuleDeclaration moduleName exportSet =
    let
        lineStart =
            "module " ++ (String.join "." moduleName) ++ " exposing "
    in
        case exportSet of
            AllExport ->
                defaultLine <| lineStart ++ "(..)"

            _ ->
                Err "Not supported export set to print!"


printFunctionArgs args =
    let
        lineEnding =
            defaultLine "="
    in
        (case args of
            [] ->
                lineEnding

            l ->
                List.foldr (\item accum -> (printExpression initContext item) +> accum) lineEnding l
        )


isSimpleExpression : Expression -> Bool
isSimpleExpression e =
    case e of
        Variable _ ->
            True

        Integer _ ->
            True

        String _ ->
            True

        Access _ _ ->
            True

        _ ->
            False


requireBraces e =
    case e of
        List _ ->
            False

        Tuple _ ->
            False

        Record _ ->
            False

        _ ->
            True


isLeftAssoc binOp =
    case binOp of
        Variable [ "|>" ] ->
            True

        Variable [ "<|" ] ->
            False

        _ ->
            True
