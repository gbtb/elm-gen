module AliasDecodersAndEncoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Alias exposing (Tasks)


tasksDecoder : JD.Decoder Tasks
tasksDecoder =
    JD.list JD.int


tasksEncoder : Tasks -> JE.Value
tasksEncoder value =
    listEncoder JE.int
