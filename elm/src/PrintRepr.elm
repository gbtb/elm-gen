module PrintRepr exposing (..)

import Utils exposing (..)


type PrintRepr
    = Line Int String
    | Lines (List PrintRepr)


defaultLine s =
    Ok <| Line 0 s


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


makeLines : Result x PrintRepr -> Result x PrintRepr -> Result x PrintRepr
makeLines arg1 arg2 =
    Result.map2
        (\arg1 arg2 ->
            case ( arg1, arg2 ) of
                ( Line _ _, Line _ _ ) ->
                    Lines [ arg1, arg2 ]

                ( Line _ _, Lines l ) ->
                    Lines (arg1 :: l)

                ( Lines l, Line _ _ ) ->
                    Lines (l ++ [ arg2 ])

                ( Lines l1, Lines l2 ) ->
                    Lines (l1 ++ l2)
        )
        arg1
        arg2


(+>) a b =
    flattenResult <|
        Result.map2
            (\a b ->
                case ( a, b ) of
                    ( Line i str1, Line _ str2 ) ->
                        Ok <| Line i (str1 ++ " " ++ str2)

                    ( Line i str1, Lines lines ) ->
                        let
                            identedLines =
                                List.map (\printRepr -> ident i printRepr) lines
                        in
                            Ok <| Lines (a :: identedLines)

                    _ ->
                        Err <| "Failed to concatenate! " ++ toString ( a, b )
            )
            a
            b
infixl 5 +>


(:>) a b =
    flattenResult <|
        Result.map2
            (\a b ->
                case ( a, b ) of
                    ( Line i str1, Line _ str2 ) ->
                        Ok <| Line i (str1 ++ str2)

                    ( Line i str1, Lines lines ) ->
                        let
                            identedLines =
                                List.map (\printRepr -> ident i printRepr) lines
                        in
                            Ok <| Lines (a :: identedLines)

                    _ ->
                        Err <| "Failed to concatenate wo spaces! " ++ toString a ++ " | " ++ toString b
            )
            a
            b
infixl 5 :>


join str lst =
    lst |> List.intersperse (defaultLine str) |> List.foldl (\item accum -> accum :> item) (defaultLine "")


prepend line printRepr =
    flattenResult <|
        Result.map2
            (\line printRepr ->
                case line of
                    Line _ str1 ->
                        case printRepr of
                            Line i str2 ->
                                Ok <| Line i (str1 ++ " " ++ str2)

                            Lines ((Line i str2) :: cons) ->
                                Ok <| Lines <| (Line i (str1 ++ " " ++ str2)) :: cons

                            _ ->
                                Err "Incorrect PrinterRepresentation to prepend to!"

                    _ ->
                        Err <| "Cannot prepend bunch of lines! " ++ toString line
            )
            line
            printRepr


braces printRepr =
    Result.andThen
        (\printRepr ->
            case printRepr of
                Line i str ->
                    Ok <| Line i ("(" ++ str ++ ")")

                Lines ((Line i str) :: cons) ->
                    (Line i ("(" ++ str) :: cons) ++ [ Line i ")" ] |> Lines |> Ok

                Lines [] ->
                    Err "Empty lines print repr"

                Lines _ ->
                    Err "Lines has nested Lines, should be flattened!"
        )
        printRepr
