module DependentOnOtherFileDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import DependentOnOtherFile exposing (DependentType(..))
import DependentTypes exposing (Basic(..), Record)


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


basicEncoder : Basic -> JE.Value
basicEncoder value =
    case value of
        Trivial ->
            JE.object [ ( "Trivial", JE.null ) ]

        Cons1 v1 ->
            JE.object [ ( "Cons1", JE.int v1 ) ]

        Cons2 v1 ->
            JE.object [ ( "Cons2", listEncoder JE.string v1 ) ]


dependentTypeEncoder : DependentType -> JE.Value
dependentTypeEncoder value =
    case value of
        A ->
            JE.object [ ( "A", JE.null ) ]

        B v1 ->
            JE.object [ ( "B", basicEncoder v1 ) ]

        C v1 ->
            JE.object [ ( "C", recordEncoder v1 ) ]


listEncoder : a -> JE.Value -> List a -> JE.Value
listEncoder encoder value =
    JE.list <|
        List.map encoder value


recordEncoder : Record -> JE.Value
recordEncoder value =
    JE.object
        [ ( "field1", listEncoder JE.float value.field1 )
        , ( "field2", basicEncoder value.field2 )
        ]
