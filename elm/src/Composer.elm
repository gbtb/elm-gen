module Composer exposing (..)

import Ast exposing (..)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (..)
import Transformation exposing (genDecoderForRecord, genDecoder, defaultContext)
import Printer exposing (printStatement)
import PrintRepr exposing (PrintRepr(..), produceString, (+>))
import List.Extra as List
import Set
import Utils exposing (..)


composeFile : List Statement -> String
composeFile statements =
    let
        records =
            List.filter recordsFilter statements

        moduleDeclaration =
            List.find moduleDeclarationFilter statements |> fromJust "Cannot find module declaration!"

        moduleName =
            getModuleName moduleDeclaration
    in
        String.join "\n" <|
            List.map (produceString 4) <|
                [ printStatement <| makeDecodersModuleDecl moduleDeclaration
                , emptyLine
                ]
                    ++ printImports moduleName (getTypes statements)
                    ++ [ emptyLine, emptyLine ]
                    ++ printDecoders records
                    ++ [ emptyLine ]


getTypes =
    List.filterMap
        (\s ->
            case s of
                TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
                    Just <| (flip TypeExport) Nothing consName

                _ ->
                    Nothing
        )


printDecoders records =
    List.concatMap (genDecoder defaultContext) records |> List.map printStatement


getModuleName s =
    case s of
        ModuleDeclaration m _ ->
            m

        _ ->
            Debug.crash "Not a module name!"


printImports sourceModuleName importedTypes =
    [ ImportStatement [ "Json", "Decode" ] (Just "JD") (Nothing)
    , ImportStatement [ "Json", "Decode", "Pipeline" ] (Just "JD") (Nothing)
    , ImportStatement [ "Json", "Encode" ] (Just "JE") (Nothing)
    , ImportStatement sourceModuleName (Nothing) (Just <| SubsetExport importedTypes)
    ]
        |> List.map (printStatement)


makeDecodersModuleDecl stmt =
    case stmt of
        ModuleDeclaration moduleName _ ->
            let
                newModuleName =
                    List.updateAt (List.length moduleName - 1) (\x -> x ++ "Decoders") moduleName
                        |> fromJust "Impossibru!"
            in
                ModuleDeclaration newModuleName AllExport

        _ ->
            Debug.crash "Incorrect statement kind was passed!"


recordsFilter s =
    case s of
        TypeAliasDeclaration (TypeConstructor [ consName ] []) (TypeRecord r) ->
            True

        _ ->
            False


moduleDeclarationFilter s =
    True


emptyLine =
    Line 0 ""
