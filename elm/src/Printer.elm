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
    }


initContext =
    { printedBinOp = False
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
                printExpression defaultContext func
                    +> if isSimpleExpression arg then
                        printExpression defaultContext arg
                       else
                        printExpression defaultContext arg
                            |> (if requireBraces arg then
                                    braces
                                else
                                    identity
                               )
                            |> ident 1

            BinOp op arg1 arg2 ->
                let
                    rightPart argContext =
                        prepend (printExpression defaultContext op) (printExpression argContext arg2)
                in
                    if (isSimpleExpression arg2) then
                        prepend (printExpression defaultContext arg1) (rightPart initContext)
                    else
                        makeLines (printExpression defaultContext arg1) <|
                            if not context.printedBinOp then
                                ident 1 <| rightPart { context | printedBinOp = True }
                            else
                                rightPart { context | printedBinOp = True }

            Access (Variable [ prefix ]) names ->
                Line 0 (String.join "." (prefix :: names))

            List exprList ->
                printList context exprList

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

        _ ->
            Debug.crash "Cannot print this type(yet?)"


printList : PrintContext -> List Expression -> PrintRepr
printList ctx exprList =
    case exprList of
        [] ->
            Line 0 "[]"

        --[ a ] ->
        --    prepend (Line 0 "[") (printExpression ctx a)
        h :: cons ->
            makeLines
                (List.foldl (\accum item -> makeLines item accum)
                    (Line 0 "[" +> printExpression ctx h)
                    (List.map (\expr -> printExpression ctx expr |> prepend (Line 0 ",")) cons)
                )
                (Line 0 "]")


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
