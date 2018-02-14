module PrintRepr exposing (..)


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


makeLines : PrintRepr -> PrintRepr -> PrintRepr
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
infixl 5 +>


(:>) a b =
    case ( a, b ) of
        ( Line i str1, Line _ str2 ) ->
            Line i (str1 ++ str2)

        ( Line i str1, Lines lines ) ->
            let
                identedLines =
                    List.map (\printRepr -> ident i printRepr) lines
            in
                Lines (a :: identedLines)

        _ ->
            Debug.crash "Failed to concatenate!"
infixl 5 :>


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


braces printRepr =
    case printRepr of
        Line i str ->
            Line i ("(" ++ str ++ ")")

        Lines ((Line i str) :: cons) ->
            (Line i ("(" ++ str) :: cons) ++ [ Line i ")" ] |> Lines

        Lines [] ->
            Debug.crash "Empty lines print repr"

        Lines _ ->
            Debug.crash "Lines has nested Lines, should be flattened!"
