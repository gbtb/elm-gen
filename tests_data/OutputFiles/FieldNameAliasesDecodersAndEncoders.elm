module FieldNameAliasesDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import FieldNameAliases exposing (R)


rDecoder : JD.Decoder R
rDecoder =
    JD.succeed R
        |> JD.required "_id" JD.string
        |> JD.required "_rev" JD.string
        |> JD.required "payload" (JD.list JD.int)


listEncoder : (a -> JE.Value) -> List a -> JE.Value
listEncoder encoder value =
    JE.list <|
        List.map encoder value


rEncoder : R -> JE.Value
rEncoder value =
    JE.object
        [ ( "_id", JE.string value.id )
        , ( "_rev", JE.string value.rev )
        , ( "payload", listEncoder JE.int value.payload )
        ]
