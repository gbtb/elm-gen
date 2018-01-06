module DependentOnOtherFileDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import DependentOnOtherFile exposing (DependentType)
import DependentTypes exposing (Basic, Record)


basicDecoder : JD.Decoder Basic
basicDecoder =
    JD.oneOf
        [ JD.field "Trivial" (JD.succeed Trivial)
        , JD.field "Cons1" (JD.map Cons1 JD.int)
        , JD.field "Cons2" (JD.map Cons2 (JD.list JD.string))
        ]


dependentTypeDecoder : JD.Decoder DependentType
dependentTypeDecoder =
    JD.oneOf
        [ JD.field "A" (JD.succeed A)
        , JD.field "B" (JD.map B basicDecoder)
        , JD.field "C" (JD.map C recordDecoder)
        ]


recordDecoder : JD.Decoder Record
recordDecoder =
    JD.decode Record
        |> JD.required "field1" (JD.list JD.float)
        |> JD.required "field2" basicDecoder
