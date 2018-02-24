module TransitiveImportDecoders exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import TransitiveImport exposing (A(..), R)
import TransitiveImport2 exposing (NestedR, NestedA(..))


aDecoder : JD.Decoder A
aDecoder =
    JD.oneOf [ JD.field "A" (JD.succeed A) ]


nestedADecoder : JD.Decoder NestedA
nestedADecoder =
    JD.oneOf [ JD.field "NestedA" (JD.succeed NestedA) ]


nestedRDecoder : JD.Decoder NestedR
nestedRDecoder =
    JD.decode NestedR
        |> JD.required "a" nestedADecoder


rDecoder : JD.Decoder R
rDecoder =
    JD.decode R
        |> JD.required "a" aDecoder
        |> JD.required "b" nestedRDecoder
