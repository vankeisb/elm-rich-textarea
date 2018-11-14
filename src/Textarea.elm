module Textarea exposing
    ( Model
    , Msg
    , init
    , view
    , update
    , subscriptions
    , attributedRenderer
    )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Json.Decode as Json
import Range exposing (Range)
import Styles exposing (..)



type alias ModelData s =
    { text: String
    , selection: Maybe Range
    , styles: Styles s
    , styledTexts: List (List (StyledText s))
    }


type Model s =
    Model (ModelData s)


type Msg
    = OnInput String Int Int
    | OnKeyDown Int Int Int
    | OnKeyUp Int Int Int
    | NoOp


init : (Model s, Cmd Msg)
init =
    (
        Model
            { text = "initial text"
            , selection = Nothing
            , styles = Styles.empty
            , styledTexts = []
            }
            |> computeStyledTexts
    , Cmd.none
    )


textareaId =
    "elm-textarea"



{-
    Applies styles to a string at a given offset. Selection
    range is also passed for drawing the selection.
-}
type alias Renderer s m = String -> Int -> Maybe Range -> List s -> Html m


view : (Msg -> m) -> Renderer s m -> Model s -> Html m
view lift renderer (Model d) =
    let
        lines =
            d.styledTexts
                |> List.map
                    (\lineElems ->
                        div
                            []
                            ( lineElems
                                |> List.map
                                    (\e ->
                                        renderer
                                            e.text
                                            (Range.getFrom e.range)
                                            d.selection
                                            e.styles
                                    )
                            )
                    )
    in
    div
        []
        [ div
            [ style "border" "1px solid black"
            , style "height" "200px"
            , style "width" "500px"
            , style "white-space" "pre"
            ]
            lines
        , Html.map lift <|
            textarea
                [ value d.text
                , id textareaId
                , on "input" <|
                    Json.map3 OnInput
                        (Json.at [ "target", "value" ] Json.string)
                        (Json.at [ "target", "selectionStart" ] Json.int)
                        (Json.at [ "target", "selectionEnd" ] Json.int)
                , on "keydown" <| keyDecoder OnKeyDown
                , on "keyup" <| keyDecoder OnKeyUp
                ]
                []
        ]


keyDecoder : (Int -> Int -> Int -> Msg) -> Json.Decoder Msg
keyDecoder msg =
    Json.map3 msg
        (Json.at [ "keyCode" ] Json.int)
        (Json.at [ "target", "selectionStart" ] Json.int)
        (Json.at [ "target", "selectionEnd" ] Json.int)



type alias Highlighter s = String -> List (Range, s)


computeStyles : Highlighter s -> Model s -> Model s
computeStyles highlighter (Model d) =
    Model
        { d
            | styles =
                Styles.empty
                    |> Styles.addStyles ( highlighter d.text )
        }
        |> computeStyledTexts



computeStyledTexts : Model s -> Model s
computeStyledTexts (Model d) =
    Model
        { d
            | styledTexts =
                String.split "\n" d.text
                    |> List.foldl
                        (\line (offset, res) ->
                            let
                                lineWithLf =
                                    line ++ "\n"
                            in
                            ( offset + (String.length lineWithLf)
                            , res ++
                                [ Styles.applyToText
                                    lineWithLf
                                    offset
                                    d.styles
                                ]
                            )
                        )
                        (0, [])
                    |> Tuple.second
        }



noCmd m =
    (m, Cmd.none)


update : Highlighter s -> Msg -> Model s -> (Model s, Cmd Msg)
update hl msg (Model model) =
    case Debug.log "msg" msg of
        OnInput s start end ->
            Model
                { model
                    | text =
                        s
                    , selection =
                        Just (Range.range start end)
                }
                |> computeStyles hl
                |> noCmd

        OnKeyDown keyCode start end ->
            onKey hl keyCode start end model

        OnKeyUp keyCode start end ->
            onKey hl keyCode start end model

        NoOp ->
            (Model model, Cmd.none)



onKey : Highlighter s -> Int -> Int -> Int -> ModelData s -> (Model s, Cmd Msg)
onKey hl keyCode start end d =
    Model
        { d
            | selection =
                Just (Range.range start end)
        }
        |> computeStyles hl
        |> noCmd



subscriptions : Model s -> Sub Msg
subscriptions model =
    Sub.none



addStyles : List (Range, s) -> Model s -> Model s
addStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.addStyles styles d.styles
        }



attributedRenderer : (List s -> List (Html.Attribute m)) -> Renderer s m
attributedRenderer attrsSupplier str from selRange styles =
    let
        dataFrom f =
            attribute "data-from" <| (String.fromInt f)

        attrs =
            attrsSupplier styles

        charAttrs i =
            let
                (isSelected, isCaretLeft) =
                    selRange
                        |> Maybe.map
                            (\r ->
                                ( Range.contains (from + i) r
                                , Range.isCaret (from + i) r
                                )
                            )
                        |> Maybe.withDefault
                            ( False
                            , False
                            )

            in
            (
                [ dataFrom <| from + i
                , style "display" "inline-block"
                , style "position" "relative"
                ] ++
                    (
                        if isSelected then
                            [ style "background-color" "lightblue" ]
                        else
                            []
                    )
            , isCaretLeft
            )

    in
    span
        attrs
        ( str
            |> String.toList
            |> List.indexedMap
                (\i c ->
                    let
                        (ca, isCaretLeft) =
                            charAttrs i
                    in
                    div
                        ca
                        [
                            if isCaretLeft then
                                div
                                    [ style "border-left" "1px solid black"
                                    , style "position" "absolute"
                                    , style "top" "0"
                                    , style "left" "0"
                                    , style "bottom" "0"
                                    , style "width" "0px"
                                    , style "box-sizing" "border-box"
                                    ]
                                    []
                            else
                                text ""
                        , text (String.fromChar c)
                        ]
                )
        )
