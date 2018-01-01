module Printer exposing (..)

import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)


type alias PrinterSettings =
    { tabs : Int
    , tabWidth : Int
    }


type PrintRepr
    = Line Int String
    | Lines (List PrintRepr)


produceString : Int -> PrintRepr -> String
produceString tabWidth printRepr =
    case printRepr of
        Line i str ->
            (String.repeat (i * tabWidth) " ") ++ str

        Lines l ->
            List.map (produceString tabWidth) l |> String.join "\n"


ident i printRepr =
    case printRepr of
        Line j str ->
            Line (i + j) str

        Lines lines ->
            Lines <| List.map (\printRepr -> ident i printRepr) lines


makeLines arg1 arg2 =
    case ( arg1, arg2 ) of
        ( Line _ _, Line _ _ ) ->
            Lines [ arg1, arg2 ]

        ( Line _ _, Lines l ) ->
            Lines (arg1 :: l)

        ( Lines l, Line _ _ ) ->
            Lines (l ++ [ arg2 ])

        ( Lines l1, Lines l2 ) ->
            Lines (l1 ++ l2)


(+>) a b =
    case ( a, b ) of
        ( Line i str1, Line _ str2 ) ->
            Line i (str1 ++ " " ++ str2)

        ( Line i str1, Lines lines ) ->
            let
                identedLines =
                    List.map (\printRepr -> ident i printRepr) lines
            in
                Lines (a :: identedLines)

        _ ->
            Debug.crash "Failed to concatenate!"


prepend line printRepr =
    case line of
        Line _ str1 ->
            case printRepr of
                Line i str2 ->
                    Line i (str1 ++ " " ++ str2)

                Lines ((Line i str2) :: cons) ->
                    Lines <| (Line i (str1 ++ " " ++ str2)) :: cons

                _ ->
                    Debug.crash "Incorrect PrinterRepresentation to prepend to!"

        _ ->
            Debug.crash "Cannot prepend bunch of lines!"


infixr 5 +<


infixl 5 +>


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
                printExpression defaultContext func +> printExpression defaultContext arg

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

        _ ->
            False
