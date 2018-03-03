module ReadConfig exposing (..)

import Model exposing (..)
import ModelDecoders exposing (..)
import Json.Decode as JD
import List
import List.Extra as List
import Config exposing (..)
import Path.Generic exposing (..)
import Regex
import Utils exposing (..)


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


makeOutputFileName : Config -> String -> String
makeOutputFileName config name =
    let
        fileName =
            List.last <|
                Regex.split
                    Regex.All
                    (Regex.regex <| "[/]")
                    name

        baseName =
            fileName
                |> Maybe.andThen
                    (\fn ->
                        List.head <|
                            Regex.split
                                Regex.All
                                (Regex.regex "[.]")
                                fn
                    )

        newBaseName =
            Maybe.map (\baseName -> config.outputFileName.prefix ++ baseName ++ config.outputFileName.suffix) <|
                case config.outputFileName.providedName of
                    DontTouch ->
                        baseName

                    Replace str ->
                        Just str
    in
        fromJust ("Failed to create output file name from: " ++ name) <|
            Maybe.map2
                (\baseName newBaseName ->
                    Regex.replace (Regex.AtMost 1) (Regex.regex <| baseName ++ ".elm") (\_ -> newBaseName ++ ".elm") name
                )
                baseName
                newBaseName


{-| Simply replaces last part of module declaration of input file with output file name.
    TODO: Handle cases with relative paths and stuff?
-}
getModuleNameFromOutputFileName : String -> String -> Maybe (List String)
getModuleNameFromOutputFileName rootDir outFile =
    let
        rootDir_ =
            if String.endsWith "/" rootDir then
                String.dropRight 1 rootDir
            else
                rootDir

        split =
            Regex.split Regex.All (Regex.regex "[/\\\\]")

        rootPath =
            split rootDir_

        outPath =
            split outFile
    in
        getModuleNameHelper rootPath outPath
            |> Maybe.andThen
                (\outPath ->
                    Maybe.map2
                        (\init last -> init ++ [ last ])
                        (List.init outPath)
                        (List.last outPath
                            |> Maybe.andThen
                                (\fileName ->
                                    String.split "." fileName |> List.head
                                )
                        )
                )


getModuleNameHelper rootPath outPath =
    case rootPath of
        x :: xs ->
            case outPath of
                y :: ys ->
                    getModuleNameHelper xs ys

                [] ->
                    Nothing

        [] ->
            Just outPath


getNameFunc : NameModification -> String -> String
getNameFunc nameMod name =
    let
        name_ =
            case nameMod.providedName of
                DontTouch ->
                    name

                Replace str ->
                    str
    in
        nameMod.prefix ++ name_ ++ nameMod.suffix


defaultDecoderNameFunc =
    getNameFunc { initNameMod | suffix = "Decoder" }


defaultEncoderNameFunc =
    getNameFunc { initNameMod | suffix = "Encoder" }
