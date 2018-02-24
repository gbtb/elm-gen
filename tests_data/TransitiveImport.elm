module TransitiveImport exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import TransitiveImport2 exposing (NestedA(..), NestedR)


type A
    = A


type alias R =
    { a : A, b : NestedR }
