module AliasDecodersAndEncoders exposing (..)

import Alias exposing (Tasks)
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


tasksDecoder : JD.Decoder Tasks
tasksDecoder =
    JD.list JD.int


listEncoder : (a -> JE.Value) -> List a -> JE.Value
listEncoder encoder value =
    JE.list <|
        List.map encoder value


tasksEncoder : Tasks -> JE.Value
tasksEncoder value =
    listEncoder JE.int value
