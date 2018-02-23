module ConfigTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Composer exposing (..)
import StatementFilters exposing (..)
import Model exposing (..)
import Config exposing (..)
import Dict
import Set


suite : Test
suite =
    let
        config1 =
            """
        {
            "encodersName": {
                "prefix": "encode"
            },
            "decodersName": {
                "suffix": "decoder"
            },
            "outputFileName" {
                "providedName": {"replace": "Decoders"}
            }
        }
    """
    in
        describe "Config decoder: "
            [ test "can decode example config" <|
                \_ ->
                    Expect.equal (readConfig config1)
                        { genCommand = Nothing
                        , encodersName = { prefix = "encode", suffix = "", providedName = DontTouch }
                        , decodersName = { prefix = "", suffix = "decoder", providedName = DontTouch }
                        , outputFileName = { prefix = "", suffix = "", providedName = Replace "Decoders" }
                        , unionTypeGeneratorFeatures = [ TrivialString ]
                        }
            ]
