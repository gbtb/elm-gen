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
    }


initContext =
    { printedBinOp = False
    , flatList = False
    , nestedTypeApplication = False
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
                            |> Result.map (ident 1)

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
    defaultLine <| name ++ " : " ++ printType initContext type_


printType ctx type_ =
    case type_ of
        TypeConstructor qualifiedType typesList ->
            String.join "." qualifiedType
                ++ (if List.length typesList > 0 then
                        " "
                    else
                        ""
                   )
                ++ (String.join " " <| List.map (printType { ctx | nestedTypeApplication = False }) typesList)

        TypeTuple typesList ->
            "("
                ++ (List.map (printType { ctx | nestedTypeApplication = False }) typesList |> String.join " ")
                ++ ")"

        TypeApplication tc1 tc2 ->
            let
                str =
                    printType { ctx | nestedTypeApplication = True } tc1
                        ++ " -> "
                        ++ printType { ctx | nestedTypeApplication = False } tc2
            in
                if ctx.nestedTypeApplication then
                    "(" ++ str ++ ")"
                else
                    str

        TypeVariable name ->
            name

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
    in
        if (isSimpleExpression arg2) then
            prepend (leftPart op initContext arg1) (rightPart op initContext arg2)
        else
            makeLines (leftPart op initContext arg1) <|
                if not context.printedBinOp then
                    Result.map (ident 1) <| rightPart op { context | printedBinOp = True } arg2
                else
                    rightPart op { context | printedBinOp = True } arg2


printList : PrintContext -> List Expression -> Result String PrintRepr
printList ctx exprList =
    case exprList of
        [] ->
            defaultLine "[]"

        [ a ] ->
            (defaultLine "[" +> (printExpression ctx a) +> defaultLine "]")

        h :: cons ->
            if ctx.flatList then
                (List.foldl (\accum item -> item :> accum)
                    (defaultLine "[" +> printExpression ctx h)
                    (List.map (\expr -> (defaultLine ",") +> printExpression ctx expr) cons)
                )
                    +> (defaultLine "]")
            else
                makeLines
                    (List.foldl (\accum item -> makeLines item accum)
                        (defaultLine "[" +> printExpression ctx h)
                        (List.map (\expr -> printExpression ctx expr |> prepend (defaultLine ",")) cons)
                    )
                    (defaultLine "]")


printTuple : PrintContext -> List Expression -> Result String PrintRepr
printTuple ctx exprList =
    case exprList of
        [] ->
            defaultLine "()"

        h :: cons ->
            (List.foldl (\accum item -> item :> accum)
                (defaultLine "(" +> printExpression ctx h)
                (List.map (\expr -> printExpression { ctx | flatList = True } expr |> prepend (defaultLine ",")) cons)
            )
                +> (defaultLine ")")


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
            "import " ++ (String.join "." moduleName)
    in
        defaultLine <|
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
