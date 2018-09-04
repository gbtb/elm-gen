module WithBothDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import WithBoth exposing (DependentType(..), Record, basicDecode, basicEncoder)


dependentTypeDecoder : JD.Decoder DependentType
dependentTypeDecoder =
    JD.oneOf
        [ JD.field "A" (JD.succeed A)
        , JD.field "B" (JD.map B basicDecode)
        , JD.field "C" (JD.map C recordDecoder)
        ]


recordDecoder : JD.Decoder Record
recordDecoder =
    JD.succeed Record
        |> JD.required "field1" (JD.list JD.float)
        |> JD.required "field2" basicDecode


dependentTypeEncoder : DependentType -> JE.Value
dependentTypeEncoder value =
    case value of
        A ->
            JE.object [ ( "A", JE.null ) ]

        B v1 ->
            JE.object [ ( "B", basicEncoder v1 ) ]

        C v1 ->
            JE.object [ ( "C", recordEncoder v1 ) ]


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
