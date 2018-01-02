port module Main exposing (main)

{-| This is main module for all the parsing and transforming work for decoders to make.

#Main func
@docs main
-}

--required to interop with js

import Json.Decode


--

import Platform exposing (program)
import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder)
import Printer exposing (printStatement)
import List.Extra as List
import Composer exposing (composeFile)
import PrintRepr exposing (produceString, (+>), PrintRepr(..))


type alias Model =
    { string : String
    }


type Msg
    = AddString String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddString string ->
            let
                ast =
                    parseModule operators string
            in
                ( model
                , output <|
                    case ast of
                        Err _ ->
                            Debug.crash "Failed to parse module!"

                        Ok ( _, _, statements ) ->
                            --Debug.crash (toString statements)
                            composeFile statements
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    input AddString


{-| Main func as it is
-}
main : Program Never Model Msg
main =
    program
        { init = ( { string = "" }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port output : String -> Cmd msg


port input : (String -> msg) -> Sub msg
