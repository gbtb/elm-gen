port module Main exposing (main)

{-| This is main module for all the parsing and transforming work for decoders to make.

#Main func
@docs main
-}

import Platform exposing (program)
import Ast exposin (..)
import Transformation exposing (genDecoderForRecord)


type alias Model =
    { strings : List String
    }


type Msg
    = AddStrings (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddStrings strings ->
            let
                ast =
                    parseModule operators String.join "\n" strings
            in
                case ast of
                    Err _ ->
                        Debug.crash "Failed to parse module!"

                    Ok (ParseOk () statements) ->
                        let
                            records =
                                List.filter statementFilter statements
                        in
                            case records of
                                [ TypeAliasDeclaration (TypeConstructor [ consName ] []) r ] ->
                                    genDecoderForRecord consName "" r

                                _ ->
                                    Debug.crash "Waiting for one and only record!"


statementFilter s =
    case s of
        TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
            True

        _ ->
            False



--SpawnOutput->(model, output model.strings)


subscriptions : Model -> Sub Msg
subscriptions _ =
    input AddStrings


{-| Main func as it is
-}
main : Program Never Model Msg
main =
    program
        { init = ( { strings = [] }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port output : List String -> Cmd msg


port input : (List String -> msg) -> Sub msg
