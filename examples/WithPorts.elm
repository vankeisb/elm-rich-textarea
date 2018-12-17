port module WithPorts exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Platform.Cmd exposing (Cmd)
import Range exposing (Range)
import Task
import Textarea

{-
    Using the textarea with ports
-}

type Msg
    = TextareaMsg Textarea.Msg
    | TextClicked
    | OnParseResult D.Value
    | OnPredictResult D.Value


{-
    Custom user styles
-}
type MyStyle
    = Keyword
    | Identifier


type alias MyPrediction = String


{-
    Your Model should keep the textarea's Model, that's parent/child...
-}
type alias Model =
    { textareaModel : Textarea.Model MyStyle MyPrediction
    , blah: String
    }


init : String -> ( Model, Cmd Msg )
init idPrefix =
    let
        ( m, c ) =
            Textarea.init <|
                Textarea.defaultInitData
                    idPrefix
                    "let\n  foo = 1\nin\n  foo + bar"
    in
    ( { textareaModel = m
        , blah = ""
      }
    , Cmd.map TextareaMsg c
    )


config: Textarea.Config MyStyle MyPrediction Msg
config =
    { lift = TextareaMsg
    , highlighter = highlighter
    , predictionConfig =
        Just
            { text = identity
            , icon =
                \pred ->
                    if pred == "foo" || pred == "bar" then
                        Just <| text "λ"
                    else
                        Nothing
            }
    }



view : Model -> Html Msg
view model =
    div
        [ style "width" "400px"
        , style "height" "200px"
        , style "position" "relative"
        , style "border" "1px solid lightgray"
        ]
        [ Textarea.view config model.textareaModel
        ]


highlighter : List MyStyle -> List (Html.Attribute Msg)
highlighter myStyles =
    myStyles
        |> List.foldl
            (\myStyle attrs ->
                case myStyle of
                    Keyword ->
                        attrs
                            ++ [ style "color" "grey"
                               , style "font-weight" "bold"
                               , onClick TextClicked
                               ]

                    Identifier ->
                        attrs
                            ++ [ style "color" "#C086D0"
                               ]
            )
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextareaMsg sub ->
            let
                ( tm, c, o ) =
                    Textarea.update sub model.textareaModel

                parseCmd =
                    case o of
                        Just (Textarea.RequestHighlight hr) ->
                            highlight <| Textarea.encodeHighlightRequest hr

                        Just (Textarea.RequestPrediction pr) ->
                            predict <| Textarea.encodePredictionRequest pr

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | textareaModel =
                    tm
              }
            , Cmd.batch
                [ Cmd.map TextareaMsg c
                , parseCmd
                ]
            )

        OnParseResult v ->
            let
                hlr =
                    D.decodeValue
                        (Textarea.highlightResponseDecoder myStyleDecoder)
                        v
            in
            case hlr of
                Ok highlightResponse ->
                    (
                        { model
                            | textareaModel =
                                Textarea.applyStyles
                                    highlightResponse.id
                                    highlightResponse.styles
                                    model.textareaModel
                        }
                    , Cmd.none
                    )

                Err e ->
                    let
                        x =
                            Debug.log "failed to decode highlight response" (Debug.toString e)
                    in
                    (model, Cmd.none)


        OnPredictResult v ->
            let
                pr =
                    D.decodeValue
                        (Textarea.predictResponseDecoder D.string)
                        v
            in
            case Debug.log "pr" pr of
                Ok predictResponse ->
                    let
                        (tm, tc) =
                            Textarea.applyPredictions
                                predictResponse.predictions
                                model.textareaModel
                    in
                    (
                        { model
                            | textareaModel =
                                tm
                        }
                    , Cmd.map TextareaMsg tc
                    )

                Err e ->
                    let
                        x =
                            Debug.log "failed to decode prediction response" (Debug.toString e)
                    in
                    (model, Cmd.none)


        TextClicked ->
            ( model, Cmd.none )


{-
    Ports used for parsing with JS
-}

port highlight: E.Value -> Cmd m


port onHighlightResponse: (D.Value -> m) -> Sub m


port predict: E.Value -> Cmd m


port onPredictResponse: (D.Value -> m) -> Sub m


myStyleDecoder: D.Decoder MyStyle
myStyleDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "keyword" ->
                        D.succeed Keyword

                    "identifier" ->
                        D.succeed Identifier

                    _ as unknownStyle ->
                        D.fail <| "Unknown style " ++ unknownStyle
            )

{-
    Subs
-}

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TextareaMsg <| Textarea.subscriptions model.textareaModel
        , onHighlightResponse OnParseResult
        , onPredictResponse OnPredictResult
        ]

