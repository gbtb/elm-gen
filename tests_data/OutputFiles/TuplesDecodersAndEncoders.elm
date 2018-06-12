module TuplesDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Tuples exposing (Coord(..))


coordDecoder : JD.Decoder Coord
coordDecoder =
    JD.oneOf
        [ JD.field "A"
            (JD.map2 A
                (JD.index 0
                    (JD.succeed (,,)
                        |> JD.custom (JD.index 0 JD.int)
                        |> JD.custom (JD.index 1 JD.int)
                        |> JD.custom (JD.index 2 JD.int)
                    )
                )
                (JD.index 1 JD.string)
            )
        , JD.field "B"
            (JD.map B
                (JD.list
                    (JD.succeed (,)
                        |> JD.custom (JD.index 0 JD.float)
                        |> JD.custom (JD.index 1 JD.float)
                    )
                )
            )
        ]


coordEncoder : Coord -> JE.Value
coordEncoder value =
    case value of
        A v1 v2 ->
            JE.object [ ( "A", JE.list [ tuple3Encoder JE.int JE.int JE.int v1, JE.string v2 ] ) ]

        B v1 ->
            JE.object [ ( "B", listEncoder (tuple2Encoder JE.float JE.float) v1 ) ]


listEncoder : (a -> JE.Value) -> List a -> JE.Value
listEncoder encoder value =
    JE.list <|
        List.map encoder value


tuple2Encoder e1 e2 ( t1, t2 ) =
    [ e1 t1
    , e2 t2
    ]
        |> JE.list


tuple3Encoder e1 e2 e3 ( t1, t2, t3 ) =
    [ e1 t1
    , e2 t2
    , e3 t3
    ]
        |> JE.list
