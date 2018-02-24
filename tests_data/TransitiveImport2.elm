module TransitiveImport2 exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


type NestedA
    = NestedA


type alias NestedR =
    { a : NestedA }
