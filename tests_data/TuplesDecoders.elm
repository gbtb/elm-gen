module TuplesDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Tuples exposing (Coord(..))


coordDecoder : JD.Decoder Coord
coordDecoder =
    JD.oneOf
        [ JD.field "A"
            (JD.map A
                (JD.succeed (,,)
                    |> JD.custom (JD.index 0 JD.int)
                    |> JD.custom (JD.index 1 JD.int)
                    |> JD.custom (JD.index 2 JD.int)
                )
            )
        ]
