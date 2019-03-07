port module WithPorts2 exposing (Model, Msg(..), MyPrediction, MyStyle(..), config, highlight, highlighter, init, myStyleDecoder, onHighlightResponse, onPredictResponse, predict, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Platform.Cmd exposing (Cmd)
import Range exposing (Range)
import Task
import Textarea2 as Textarea



{-
   Using the textarea with ports
-}


type Msg
    = TextareaMsg (Textarea.Msg MyStyle Msg)
    | TextClicked
    | OnParseResult D.Value
    | OnPredictResult D.Value
    | RequestHighlight String (Textarea.ApplyStylesFun MyStyle Msg)



{-
   Custom user styles
-}


type MyStyle
    = Keyword
    | Identifier


type alias MyPrediction =
    String



{-
   Your Model should keep the textarea's Model, that's parent/child...
-}


type alias Model =
    { textareaModel : Textarea.Model MyStyle MyPrediction
    , blah : String
    , applyStyles : Maybe (Textarea.ApplyStylesFun MyStyle Msg)
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
      , applyStyles = Nothing
      }
    , Cmd.map TextareaMsg c
    )


config : Textarea.ViewConfig MyStyle MyPrediction Msg
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


updateConfig : Textarea.UpdateConfig MyStyle MyPrediction Msg
updateConfig =
    { lift = TextareaMsg
    , highlighter = renderer
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
    , requestHighlight =
        \applyStyles text ->
            RequestHighlight text applyStyles
    }


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


view : Model -> Html Msg
view model =
    div
        []
        [ h2
            []
            [ text "Highlighting/Predictions in JS" ]
        , div
            [ style "width" "400px"
            , style "height" "200px"
            , style "position" "relative"
            , style "border" "1px solid lightgray"
            ]
            [ Textarea.view config model.textareaModel
            ]
        , p
            []
            [ text "Type "
            , code [] [ text "CTRL+SPACE" ]
            , text " to trigger prediction menu."
            ]
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
                ( tm, c ) =
                    Textarea.update updateConfig sub model.textareaModel
            in
            ( { model
                | textareaModel =
                    tm
              }
            , c
            )

        OnParseResult v ->
            let
                hlr =
                    D.decodeValue
                        (Textarea.stylesDecoder myStyleDecoder)
                        v
            in
            case hlr of
                Ok styles ->
                    ( { model
                        | applyStyles = Nothing
                      }
                    , model.applyStyles
                        |> Maybe.map
                            (\apply ->
                                styles
                                    |> Task.succeed
                                    |> Task.perform apply
                            )
                        |> Maybe.withDefault Cmd.none
                    )

                Err e ->
                    let
                        x =
                            Debug.log "failed to decode highlight response" (Debug.toString e)
                    in
                    ( model, Cmd.none )

        OnPredictResult v ->
            --            let
            --                pr =
            --                    D.decodeValue
            --                        (Textarea.predictResponseDecoder D.string)
            --                        v
            --            in
            --            case Debug.log "pr" pr of
            --                Ok predictResponse ->
            --                    let
            --                        ( tm, tc ) =
            --                            Textarea.applyPredictions
            --                                config
            --                                predictResponse
            --                                model.textareaModel
            --                    in
            --                    ( { model
            --                        | textareaModel =
            --                            tm
            --                      }
            --                    , Cmd.map TextareaMsg tc
            --                    )
            --
            --                Err e ->
            --                    let
            --                        x =
            --                            Debug.log "failed to decode prediction response" (Debug.toString e)
            --                    in
            ( model, Cmd.none )

        TextClicked ->
            ( model, Cmd.none )

        RequestHighlight text applyFun ->
            ( { model | applyStyles = Just applyFun }
            , highlight text
            )



{-
   Ports used for parsing with JS
-}


port highlight : String -> Cmd m


port onHighlightResponse : (D.Value -> m) -> Sub m


port predict : E.Value -> Cmd m


port onPredictResponse : (D.Value -> m) -> Sub m


myStyleDecoder : D.Decoder MyStyle
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
        [ -- TODO
          --Sub.map TextareaMsg <| Textarea.subscriptions model.textareaModel
          --,
          onHighlightResponse OnParseResult
        , onPredictResponse OnPredictResult
        ]
