module Printer exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import List.Extra as List
import Utils exposing (..)
import PrintRepr exposing (..)


type alias PrinterSettings =
    { tabs : Int
    , tabWidth : Int
    }


type alias PrintContext =
    { printedBinOp : Bool
    , flatList : Bool
    }


initContext =
    { printedBinOp = False
    , flatList = False
    }


printExpression : PrintContext -> Expression -> PrintRepr
printExpression context e =
    let
        defaultContext =
            initContext
    in
        case e of
            Integer i ->
                Line 0 <| toString i

            String s ->
                Line 0 <| "\"" ++ s ++ "\""

            Variable l ->
                Line 0 <| String.join "." l

            Application func arg ->
                printExpression context func
                    +> if isSimpleExpression arg then
                        printExpression context arg
                       else
                        printExpression context arg
                            |> (if requireBraces arg then
                                    braces
                                else
                                    identity
                               )
                            |> ident 1

            BinOp op arg1 arg2 ->
                printBinOp context op arg1 arg2

            Access (Variable [ prefix ]) names ->
                Line 0 (String.join "." (prefix :: names))

            List exprList ->
                printList context exprList

            Tuple exprList ->
                printTuple context exprList

            Case expr caseBranches ->
                makeLines
                    (Line 0 "case" +> (printExpression context expr) +> Line 0 "of")
                    (List.map (printCaseBranch context) caseBranches |> List.intersperse (Line -1 "") |> Lines)

            _ ->
                Debug.crash "Cant print this type of expression!"


printStatement : Statement -> PrintRepr
printStatement stmt =
    case stmt of
        FunctionDeclaration name args body ->
            let
                firstLine =
                    prepend (Line 0 name) (printFunctionArgs args)
            in
                makeLines firstLine <| ident 1 (printExpression initContext body)

        ModuleDeclaration moduleName exportSet ->
            printModuleDeclaration moduleName exportSet

        ImportStatement moduleName alias exportSet ->
            printImportStatement moduleName alias exportSet

        FunctionTypeDeclaration name type_ ->
            printFunctionTypeDeclaration name type_

        _ ->
            Debug.crash "Print of this statement is unsupported(yet?)"


printFunctionTypeDeclaration name type_ =
    Line 0 <| name ++ " : " ++ printType type_


printType type_ =
    case type_ of
        TypeConstructor qualifiedType typesList ->
            String.join "." qualifiedType
                ++ (if List.length typesList > 0 then
                        " "
                    else
                        ""
                   )
                ++ (String.join " " <| List.map printType typesList)

        TypeTuple typesList ->
            "("
                ++ (List.map printType typesList |> String.join " ")
                ++ ")"

        TypeApplication tc1 tc2 ->
            printType tc1 ++ " -> " ++ printType tc2

        TypeVariable name ->
            name

        _ ->
            Debug.crash "Cannot print this type(yet?)"


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
    in
        if (isSimpleExpression arg2) then
            prepend (leftPart op initContext arg1) (rightPart op initContext arg2)
        else
            makeLines (leftPart op initContext arg1) <|
                if not context.printedBinOp then
                    ident 1 <| rightPart op { context | printedBinOp = True } arg2
                else
                    rightPart op { context | printedBinOp = True } arg2


printList : PrintContext -> List Expression -> PrintRepr
printList ctx exprList =
    case exprList of
        [] ->
            Line 0 "[]"

        [ a ] ->
            (Line 0 "[" +> (printExpression ctx a) +> Line 0 "]")

        h :: cons ->
            if ctx.flatList then
                (List.foldl (\accum item -> item :> accum)
                    (Line 0 "[" +> printExpression ctx h)
                    (List.map (\expr -> (Line 0 ",") +> printExpression ctx expr) cons)
                )
                    +> (Line 0 "]")
            else
                makeLines
                    (List.foldl (\accum item -> makeLines item accum)
                        (Line 0 "[" +> printExpression ctx h)
                        (List.map (\expr -> printExpression ctx expr |> prepend (Line 0 ",")) cons)
                    )
                    (Line 0 "]")


printTuple : PrintContext -> List Expression -> PrintRepr
printTuple ctx exprList =
    case exprList of
        [] ->
            Line 0 "()"

        h :: cons ->
            (List.foldl (\accum item -> item :> accum)
                (Line 0 "(" +> printExpression ctx h)
                (List.map (\expr -> printExpression { ctx | flatList = True } expr |> prepend (Line 0 ",")) cons)
            )
                +> (Line 0 ")")


printCaseBranch ctx ( leftPart, rightPart ) =
    let
        l1 =
            ident 1 <| printExpression ctx leftPart +> Line 0 "->"

        l2 =
            ident 2 <| printExpression ctx rightPart
    in
        makeLines l1 l2


printImportStatement moduleName alias exportSet =
    let
        lineStart =
            "import " ++ (String.join "." moduleName)
    in
        Line 0 <|
            lineStart
                ++ (alias |> Maybe.map (\a -> " as " ++ a) |> Maybe.withDefault "")
                ++ (case exportSet of
                        Just AllExport ->
                            " exposing (..)"

                        Just (SubsetExport typesList) ->
                            " exposing ("
                                ++ (List.map printExportSet typesList |> String.join ", ")
                                ++ ")"

                        Nothing ->
                            ""

                        _ ->
                            Debug.crash "111"
                   )


printExportSet : ExportSet -> String
printExportSet es =
    case es of
        AllExport ->
            ".."

        SubsetExport nestedList ->
            List.map printExportSet nestedList |> String.join ", "

        FunctionExport name ->
            name

        TypeExport name constructors ->
            name
                ++ case constructors of
                    Nothing ->
                        ""

                    Just nestedEs ->
                        "(" ++ printExportSet nestedEs ++ ")"


printModuleDeclaration moduleName exportSet =
    let
        lineStart =
            "module " ++ (String.join "." moduleName) ++ " exposing "
    in
        case exportSet of
            AllExport ->
                Line 0 <| lineStart ++ "(..)"

            _ ->
                Debug.crash "Not supported export set to print!"


printFunctionArgs args =
    let
        lineEnding =
            Line 0 "="
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
