module WithDefaultValues exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithDecoder exposing (R(..), aDecoder)


aDecoder : JD.Decoder A
aDecoder =
    JD.oneOf
        [ JD.field "Trivial" JD.null
        , JD.field "B" (JD.map B JD.int)
        , JD.field "C" (JD.map C JD.string)
        , JD.succeed (B 3)
        ]


rDecoder : JD.Decoder R
rDecoder = 
    JD.decode R 
        |> JD.required "a" JD.int,
        |> JD.optional "b" (JD.list JD.char) ['a']


type alias R =
    { a : Int
    , b : List Char
    }
