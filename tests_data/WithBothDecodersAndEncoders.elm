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


maybeDecoder decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]


recordDecoder : JD.Decoder Record
recordDecoder =
    JD.decode Record
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


listEncoder : a -> Value -> List a -> Value
listEncoder encoder =
    JE.list <|
        List.map encoder


maybeEncoder valueEncoder value =
    case value of
        Just value ->
            valueEncoder value

        Nothing ->
            JE.null


recordEncoder : Record -> JE.Value
recordEncoder value =
    JE.object
        [ ( "field1", listEncoder JE.float value.field1 )
        , ( "field2", basicEncoder value.field2 )
        ]
