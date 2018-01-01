module ComposeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ast.Statement exposing (..)
import Ast.BinOp exposing (..)
import Ast.Expression exposing (..)
import Composer exposing (..)


suite : Test
suite =
    describe "Composer:"
        [ test "can make correct module declaration for decoder" <|
            \_ ->
                Expect.equal
                    (makeDecodersModuleDecl <| ModuleDeclaration [ "Basic" ] AllExport)
                    (ModuleDeclaration [ "BasicDecoders" ] AllExport)
        ]
