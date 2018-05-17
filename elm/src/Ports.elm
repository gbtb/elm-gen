port module Ports exposing (..)

import Json.Decode as JD
import Json.Encode as JE


port config : (String -> msg) -> Sub msg


port output : ( String, String ) -> Cmd msg


port requestFiles : List (List String) -> Cmd msg


port logMessage : String -> Cmd msg


port errorMessage : String -> Cmd msg


port input : (JE.Value -> msg) -> Sub msg
