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


{-
    Custom user styles
-}
type MyStyle
    = Keyword
    | Identifier


{-
    Your Model should keep the textarea's Model, that's parent/child...
-}
type alias Model =
    { textareaModel : Textarea.Model MyStyle
    }


init : String -> ( Model, Cmd Msg )
init idPrefix =
    let
        initialText =
            "let\n  foo = 1\nin\n  foo + bar"


        -- init the textarea : we pass the text and
        -- the styles for this text
        ( m, c ) =
            Textarea.init
                (Textarea.defaultInitData idPrefix initialText)
    in
    ( { textareaModel = m
      }
    , Cmd.map TextareaMsg c
    )


view : Model -> Html Msg
view model =
    div
        [ style "width" "400px"
        , style "height" "200px"
        , style "position" "relative"
        , style "border" "1px solid lightgray"
        ]
        [ Textarea.view
            TextareaMsg
            renderer
            model.textareaModel
        ]


renderer : List MyStyle -> List (Html.Attribute Msg)
renderer myStyles =
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


        TextClicked ->
            ( model, Cmd.none )


{-
    Ports used for parsing with JS
-}

port highlight: E.Value -> Cmd m


port onHighlightResponse: (D.Value -> m) -> Sub m


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
        ]

