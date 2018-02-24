module ReadConfig exposing (..)

import Model exposing (..)
import ModelDecoders exposing (..)
import Json.Decode as JD
import List
import Config exposing (..)


readConfig : String -> Result String Config
readConfig str =
    let
        config =
            JD.decodeString configDecoder str
    in
        Result.map
            (\config ->
                if List.isEmpty config.unionTypeGeneratorFeatures then
                    { config | unionTypeGeneratorFeatures = [ TrivialString ] }
                else
                    config
            )
            config
