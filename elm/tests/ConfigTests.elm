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
import ReadConfig exposing (..)
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
            "outputFileName": {
                "providedName": {"Replace": "Decoders"}
            },
            "unionTypeGeneratorFeatures": []
        }
    """
    in
        describe "Config decoder: "
            [ test "can decode example config" <|
                \_ ->
                    Expect.equal (readConfig config1)
                        (Ok
                            { genCommand = Nothing
                            , encodersName = { prefix = "encode", suffix = "", providedName = DontTouch }
                            , decodersName = { prefix = "", suffix = "decoder", providedName = DontTouch }
                            , outputFileName = { prefix = "", suffix = "", providedName = Replace "Decoders" }
                            , unionTypeGeneratorFeatures = [ TrivialString ]
                            }
                        )
            , test
                "make simple output file name"
              <|
                \_ ->
                    Expect.equal (makeOutputFileName initConfig "../tests_data/Test.elm")
                        "../tests_data/Test.elm"
            , test "make simple output file name with prefix" <|
                \_ ->
                    Expect.equal
                        (makeOutputFileName
                            { initConfig
                                | outputFileName = { prefix = "Pr", suffix = "", providedName = DontTouch }
                            }
                            "../tests_data/Test.elm"
                        )
                        "../tests_data/PrTest.elm"
            , test "get output module name" <|
                \_ ->
                    Expect.equal (getModuleNameFromOutputFileName [ "Main", "Module" ] "Main/NewDecoders.elm")
                        ([ "Main", "NewDecoders" ])
            , test "get output module name 2" <|
                \_ ->
                    Expect.equal (getModuleNameFromOutputFileName [ "Module" ] "./NewDecoders.elm")
                        ([ "NewDecoders" ])
            ]
