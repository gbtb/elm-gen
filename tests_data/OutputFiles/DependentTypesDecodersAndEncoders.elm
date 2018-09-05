module DependentTypesDecodersAndEncoders exposing (..)

import DependentTypes exposing (Basic(..), Record)
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


basicDecoder : JD.Decoder Basic
basicDecoder =
    JD.oneOf
        [ JD.field "Trivial" (JD.succeed Trivial)
        , JD.field "Cons1" (JD.map Cons1 JD.int)
        , JD.field "Cons2" (JD.map Cons2 (JD.list JD.string))
        ]


recordDecoder : JD.Decoder Record
recordDecoder =
    JD.decode Record
        |> JD.required "field1" (JD.list JD.float)
        |> JD.required "field2" basicDecoder


basicEncoder : Basic -> JE.Value
basicEncoder value =
    case value of
        Trivial ->
            JE.object [ ( "Trivial", JE.null ) ]

        Cons1 v1 ->
            JE.object [ ( "Cons1", JE.int v1 ) ]

        Cons2 v1 ->
            JE.object [ ( "Cons2", listEncoder JE.string v1 ) ]


listEncoder : (a -> JE.Value) -> List a -> JE.Value
listEncoder encoder value =
    JE.list <|
        List.map encoder value


recordEncoder : Record -> JE.Value
recordEncoder value =
    JE.object
        [ ( "field1", listEncoder JE.float value.field1 )
        , ( "field2", basicEncoder value.field2 )
        ]
