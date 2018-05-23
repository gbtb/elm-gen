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
import Color.Convert as Color
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
                    , output = ""
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
        (column Main
            [ minHeight (percent 95), spacingXY 0 20 ]
            [ header Header
                [ height (px 50), paddingXY 20 0 ]
                (row NoStyle
                    [ center, verticalCenter, spacingXY 20 0, width fill ]
                    [ h1 NoStyle [] <| text "elm-gen demo"
                    , link "https://github.com/gbtb/elm-gen" <|
                        image NoStyle
                            [ width (px <| scaled 2), height (px <| scaled 2) ]
                            { src =
                                "https://yodlabs.gallerycdn.vsassets.io/extensions/yodlabs/"
                                    ++ "yodlabs-githubstats/0.9.10/1499287006645/branding/logo.png"
                            , caption = "github logo"
                            }
                    ]
                )
            , mainContent MainContent
                []
                (column NoStyle
                    [ paddingXY 20 0, spacing 20, minHeight fill ]
                    [ row NoStyle
                        [ height <| fillPortion 3, spacingXY 40 20 ]
                        [ multiline InputArea
                            [ height fill, yScrollbar, paddingXY 2 2 ]
                            { onChange = debounce << InputChanged
                            , value = model.input
                            , label = labelAbove (h2 H2 [ center ] <| text "Input file content")
                            , options = [ focusOnLoad ]
                            }
                        , multiline OutputArea
                            [ height fill, yScrollbar, paddingXY 2 2 ]
                            { onChange = OutputChanged
                            , value = model.output
                            , label = labelAbove (h2 H2 [ center ] <| text "Output file content")
                            , options = []
                            }
                        ]
                    , row Log
                        [ height <| fillPortion 1, minHeight (px 200), paddingXY 2 2 ]
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


colors =
    { cyan = Color.hexToColor "#60b5cc" |> Result.withDefault Color.red
    , orange = Color.hexToColor "#f0ad00" |> Result.withDefault Color.red
    , blue = Color.hexToColor "#5a6378" |> Result.withDefault Color.red
    , red = Color.hexToColor "#ef4a0e" |> Result.withDefault Color.red
    , green = Color.hexToColor "#7fd13b" |> Result.withDefault Color.red
    }


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
    | H2
    | Main


styles =
    styleSheet
        [ style LogInfo
            [ Color.text colors.green ]
        , style Main
            [ Font.typeface [ Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" }, Font.serif ]
            , Color.text <| Color.rgba 0 0 0 0.8
            ]
        , style InputArea inputAreaSnippet
        , style OutputArea inputAreaSnippet
        , style LogError
            [ Color.text colors.red ]
        , style Log
            [ Shadow.simple
            , Color.background colors.blue
            ]
        , style Header
            [ Font.size (scaled 2)
            , Shadow.simple
            , Color.background colors.cyan
            ]
        , style H2
            [ Font.bold
            ]
        , importUrl "https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.css"
        ]


borderSnippet =
    [ Shadow.box { offset = ( 0, 0 ), blur = 6.0, size = 2, color = colors.blue }
    ]


inputAreaSnippet =
    [ Shadow.simple
    , focus <| [ Shadow.simple ] ++ borderSnippet
    , Font.typeface [ Font.monospace ]
    , Font.size (scaled 1)
    ]
        ++ borderSnippet


scaled =
    Scale.modular 16 1.618
