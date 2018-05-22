module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html
import Model
import Update
import Utils exposing (..)
import Element exposing (..)
import Element.Input exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Font as Font
import Style.Border as Border
import Style.Color as Color
import Style.Scale as Scale
import Style.Shadow as Shadow
import Color
import Control
import Control.Debounce
import Time
import Dom.Scroll exposing (toBottom)
import Dom
import Task


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
    | Scroll (Result Dom.Error ())


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
            ( { model | messages = model.messages ++ [ msg ] }, Task.attempt Scroll (toBottom "log") )
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
                                addToLog model
                                    (Update.ErrorMessage
                                        "Sorry, additional files loading is not working in this demo! Try to use console utility instead"
                                    )

                    _ ->
                        let
                            ( updated_model, cmd ) =
                                Update.update comms msg model.genModel
                        in
                            ( { model | genModel = updated_model }, Cmd.map GenMessage cmd )

            InputChanged str ->
                ( { model
                    | input = str
                    , genModel = Model.initModel
                  }
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

            Scroll _ ->
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


startInput =
    """module DependentTypes exposing (..)


type Basic
    = Trivial
    | Cons1 Int
    | Cons2 (List String)


type alias Record =
    { field1 : List Float
    , field2 : Basic
    }"""


init : ( Model, Cmd Msg )
init =
    ( { genModel = Model.initModel
      , messages = []
      , output = ""
      , input = startInput
      , debState = Control.initialState
      }
    , makeCmd <| InputChanged startInput
    )


debounce =
    Control.Debounce.trailing Deb (1 * Time.second)


text =
    Element.text


view model =
    viewport (styles)
        (column NoStyle
            [ minHeight (percent 95), spacingXY 0 20 ]
            [ header Header [ height (px 50), paddingXY 20 0 ] (el NoStyle [ center, verticalCenter ] (text "elm-gen demo"))
            , mainContent MainContent
                []
                (column NoStyle
                    [ paddingXY 20 0, spacing 20, minHeight fill ]
                    [ row NoStyle
                        [ height <| fillPortion 3, spacingXY 40 20, clip ]
                        [ multiline InputArea
                            [ height fill, yScrollbar, paddingXY 2 2 ]
                            { onChange = debounce << InputChanged
                            , value = model.input
                            , label = labelAbove (text "Input file content")
                            , options = [ focusOnLoad ]
                            }
                        , multiline OutputArea
                            [ height fill, yScrollbar, paddingXY 2 2 ]
                            { onChange = OutputChanged
                            , value = model.output
                            , label = labelAbove (text "Output file content")
                            , options = []
                            }
                        ]
                    , row Log
                        [ height <| fillPortion 1, minHeight (px 200) ]
                        [ column NoStyle
                            [ yScrollbar, id "log", width fill ]
                            (List.map
                                (\msg ->
                                    case msg of
                                        Update.LogMessage str ->
                                            row LogInfo [ width fill ] [ text str ]

                                        Update.ErrorMessage str ->
                                            row LogError [ width fill ] [ text str ]

                                        _ ->
                                            empty
                                )
                                model.messages
                            )
                        ]
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
    | LogError
    | LogInfo


styles =
    styleSheet
        [ style LogInfo
            [ Color.text Color.green ]
        , style InputArea
            [ Shadow.simple
            , focus
                [ Shadow.simple ]
            ]
        , style OutputArea
            [ Shadow.simple
            , focus [ Shadow.simple ]
            ]
        , style LogError
            [ Color.text Color.red ]
        , style Log
            [ Shadow.simple
            ]
        , style Header
            [ Font.size (scaled 2)
            , Shadow.simple
            ]
        , importUrl "https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.css"
        ]


scaled =
    Scale.modular 16 1.618
