module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html
import Model
import Update
import Utils exposing (..)
import Element exposing (..)
import Element.Input exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Control
import Control.Debounce
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { genModel : Model.Model
    , messages : List Update.PseudoCmd
    , output : String
    , input : String
    , debState : Control.State Msg
    }


type Msg
    = GenMessage Update.Msg
    | InputChanged String
    | OutputChanged String
    | Deb (Control.Control Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        comms =
            { logMessage = Update.LogMessage >> Update.PseudoCmd >> makeCmd
            , errorMessage = Update.ErrorMessage >> Update.PseudoCmd >> makeCmd
            , output = Update.Output >> Update.PseudoCmd >> makeCmd
            , requestFiles = Update.RequestFiles >> Update.PseudoCmd >> makeCmd
            }

        addToLog model msg =
            ( { model | messages = model.messages ++ [ msg ] }, Cmd.none )
    in
        case msg of
            GenMessage msg ->
                case msg of
                    Update.PseudoCmd msg ->
                        case msg of
                            Update.LogMessage _ ->
                                addToLog model msg

                            Update.ErrorMessage _ ->
                                addToLog model msg

                            Update.Output ( _, content ) ->
                                ( { model | output = content }, Cmd.none )

                            Update.RequestFiles _ ->
                                addToLog model (Update.ErrorMessage "Sorry, additional file's load is not working in this demo!")

                    _ ->
                        let
                            ( updated_model, cmd ) =
                                Update.update comms msg model.genModel
                        in
                            ( { model | genModel = updated_model }, Cmd.map GenMessage cmd )

            InputChanged str ->
                ( { model | input = str }
                , makeCmd
                    (GenMessage <|
                        Update.Parse
                            { fileContents = str
                            , fileNames = [ "/tmp/Filename.elm" ]
                            , rootDir = "/tmp"
                            , genCommand = Model.DecodersAndEncoders
                            }
                    )
                )

            OutputChanged _ ->
                ( model, Cmd.none )

            Deb msg ->
                Control.update (\s -> { model | debState = s }) model.debState msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { genModel = Model.initModel
      , messages = []
      , output = ""
      , input = ""
      , debState = Control.initialState
      }
    , Cmd.none
    )


debounce =
    Control.Debounce.trailing Deb (1 * Time.second)


text =
    Element.text


view model =
    viewport (styleSheet [])
        (column NoStyle
            []
            [ header Header [] (text "elm-gen demo")
            , mainContent MainContent
                []
                (column NoStyle
                    []
                    [ row NoStyle
                        [ height (px 400) ]
                        [ multiline InputArea
                            [ height (px 400) ]
                            { onChange = debounce << InputChanged
                            , value = model.input
                            , label = labelAbove (text "Input file content")
                            , options = [ focusOnLoad ]
                            }
                        , multiline OutputArea
                            [ height (px 400) ]
                            { onChange = OutputChanged
                            , value = model.output
                            , label = labelAbove (text "Output file content")
                            , options = []
                            }
                        ]
                    , column Log
                        []
                        (List.map
                            (\msg ->
                                case msg of
                                    Update.LogMessage str ->
                                        text str

                                    Update.ErrorMessage str ->
                                        text str

                                    _ ->
                                        empty
                            )
                            model.messages
                        )
                    ]
                )
            ]
        )


type Styles
    = NoStyle
    | Header
    | MainContent
    | InputArea
    | GenButton
    | OutputArea
    | Log
