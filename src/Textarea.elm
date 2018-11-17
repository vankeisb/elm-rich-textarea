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
import Json.Encode as Encode
import Browser.Dom as Dom
import Task
import Array
import Time exposing (Posix)


type alias ModelData s =
    { text: String
    , selection: Maybe Range
    , styles: Styles s
    , styledTexts: List (List (StyledText s))
    , focused: Bool
    , time: Posix
    , blinkStart: Posix
    }


type Model s =
    Model (ModelData s)


type Msg
    = OnInput String Int Int
    | OnKeyDown Int Int Int
    | OnKeyUp Int Int Int
    | MouseDown Int Float Int
    | BackgroundClicked
    | LineClicked Int
    | Focused (Result Dom.Error ())
    | Blurred
    | OnTime Posix
    | TriggerBlink Posix


init : Highlighter s -> String -> (Model s, Cmd Msg)
init hl s =
    (
        Model
            { text = s
            , selection = Nothing
            , styles = Styles.empty
            , styledTexts = []
            , focused = False
            , time = Time.millisToPosix 0
            , blinkStart = Time.millisToPosix 0
            }
            |> computeStyles hl
    , focusTextarea
    )


focusTextarea =
    Dom.focus textareaId
            |> Task.attempt Focused


textareaId =
    "elm-textarea"



{-
    Applies styles to a string at a given offset. Selection
    range is also passed for drawing the selection.
-}
type alias Renderer s m = String -> Int -> Maybe Range -> Bool -> Bool -> List s -> Html m


view : (Msg -> m) -> Renderer s m -> Model s -> Html m
view lift renderer (Model d) =
    let
        time =
            Time.posixToMillis d.time

        blinkStart =
            Time.posixToMillis d.blinkStart

        elapsed =
            time - blinkStart

        displayCaret =
            if elapsed < 500 then
                True
            else
                (modBy 2 (elapsed // 700)) == 0

        lines =
            d.styledTexts
                |> List.indexedMap
                    (\lineNumber lineElems ->
                        div
                            [ custom "mousedown" <|
                                Json.succeed
                                    { message = lift (LineClicked lineNumber)
                                    , preventDefault = True
                                    , stopPropagation = True
                                    }
                            ]
                            ( lineElems
                                |> List.map
                                    (\e ->
                                        renderer
                                            e.text
                                            (Range.getFrom e.range)
                                            d.selection
                                            d.focused
                                            displayCaret
                                            e.styles
                                    )
                            )
                    )

        (ss, se) =
            d.selection
                |> Maybe.map Range.getBounds
                |> Maybe.withDefault (0,0)
    in
    div
        []
        [ div
            [ style "border" "1px solid black"
            , style "height" "200px"
            , style "width" "500px"
            , style "white-space" "pre"
            , custom "mousedown" <|
                Json.succeed
                    { message = lift BackgroundClicked
                    , preventDefault = True
                    , stopPropagation = True
                    }
            ]
            lines
        , Html.map lift <|
            textarea
                [ value d.text
                , id textareaId
                , property "selectionStart" <| Encode.int ss
                , property "selectionEnd" <| Encode.int se
                , on "input" <|
                    Json.map3 OnInput
                        (Json.at [ "target", "value" ] Json.string)
                        (Json.at [ "target", "selectionStart" ] Json.int)
                        (Json.at [ "target", "selectionEnd" ] Json.int)
                , custom "keydown" <|
                    Json.map3
                        (\keyCode start end ->
                            { message = OnKeyDown keyCode start end
                            , preventDefault =
                                keyCode == 9 -- stop tab
                            , stopPropagation =
                                keyCode == 9 -- stop tab
                            }
                        )
                        (Json.at [ "keyCode" ] Json.int)
                        (Json.at [ "target", "selectionStart" ] Json.int)
                        (Json.at [ "target", "selectionEnd" ] Json.int)
                , custom "keyup" <|
                    Json.map3
                        (\keyCode start end ->
                            { message = OnKeyUp keyCode start end
                            , preventDefault =
                                keyCode == 9 -- stop tab
                            , stopPropagation =
                                keyCode == 9 -- stop tab
                            }
                        )
                        (Json.at [ "keyCode" ] Json.int)
                        (Json.at [ "target", "selectionStart" ] Json.int)
                        (Json.at [ "target", "selectionEnd" ] Json.int)
                , on "blur" <|
                    Json.succeed Blurred
                ]
                []
        ]


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
    case msg of

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
            onKey True hl keyCode start end model

        OnKeyUp keyCode start end ->
            onKey False hl keyCode start end model

        MouseDown i offsetX clientWidth ->
            -- place caret at index i or i+1, depending
            -- on the location of the click inside the
            -- char wrapper
            setCaretPos
                (
                    if offsetX < ((toFloat clientWidth) / 2) then
                        i
                    else
                        i + 1
                )
                (Model model)

        BackgroundClicked ->
            -- place caret at the end of the text
            setCaretPos
                (String.length model.text)
                (Model model)

        LineClicked lineIndex ->
            -- place caret at the end of the line
            let
                lineSize =
                    String.split "\n" model.text
                        |> List.map String.length
                        |> List.foldl
                            (\len (total, res) ->
                                let
                                    newTotal = len + total + 1
                                in
                                ( newTotal, res ++ [ newTotal ])

                            )
                            (0, [])
                        |> Tuple.second
                        |> Array.fromList
                        |> Array.get lineIndex
            in
            lineSize
                |> Maybe.map
                    (\s ->
                        setCaretPos (s - 1) (Model model)
                    )
                |> Maybe.withDefault
                    (Model model, Cmd.none)

        Focused (Ok ()) ->
            Model
                { model
                    | focused = True
                }
                |> triggerBlink


        Focused (Err _) ->
            (Model model, Cmd.none)


        Blurred ->
            ( Model
                { model
                    | focused = False
                }
            , Cmd.none
            )

        OnTime posix ->
            ( Model
                { model
                    | time =
                        posix
                }
            , Cmd.none
            )

        TriggerBlink posix ->
            ( Model
                { model
                    | time =
                        posix
                    , blinkStart =
                        posix
                }
            , Cmd.none
            )


triggerBlink : Model s -> (Model s, Cmd Msg)
triggerBlink (Model m) =
    ( Model
        { m
            | time =
                Time.millisToPosix 0
            , blinkStart =
                Time.millisToPosix 0
        }
    , Task.perform TriggerBlink Time.now
    )


setCaretPos : Int -> Model s -> (Model s, Cmd Msg)
setCaretPos i (Model d) =
    ( Model
        { d
            | selection =
                Just <| Range.range i i
        }
    , focusTextarea
    )



onKey : Bool -> Highlighter s -> Int -> Int -> Int -> ModelData s -> (Model s, Cmd Msg)
onKey isDown hl keyCode start end d =
    let
        (newText, newSel) =
            if keyCode == 9 && not isDown then
                -- TAB: insert spaces
                d.selection
                    |> Maybe.map
                        (\r ->
                            let
                                (from, to) =
                                    Range.getBounds r

                                left =
                                    String.slice 0 from d.text

                                right =
                                    String.slice to (String.length d.text) d.text
                            in
                                ( left ++ "  " ++ right
                                , Just <| Range.move 2 r
                                )
                        )
                    |> Maybe.withDefault
                        ( d.text, d.selection )
            else
                ( d.text
                , Just <| Range.range start end
                )
    in
    Model
        { d
            | selection =
                newSel
            , text =
                newText
        }
        |> computeStyles hl
        |> triggerBlink




subscriptions : Model s -> Sub Msg
subscriptions (Model model) =
    let
        isCaretSelection =
            model.selection
                |> Maybe.map Range.getBounds
                |> Maybe.map
                    (\(from,to) ->
                        from == to
                    )
                |> Maybe.withDefault False
    in
    if model.focused  && isCaretSelection then
        Time.every 100 OnTime
    else
        Sub.none



addStyles : List (Range, s) -> Model s -> Model s
addStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.addStyles styles d.styles
        }



attributedRenderer : (Msg -> m) -> (List s -> List (Html.Attribute m)) -> Renderer s m
attributedRenderer lift attrsSupplier str from selRange focused blinkDisplayCaret styles =
    let
        dataFrom f =
            attribute "data-from" <| (String.fromInt f)

        attrs =
            attrsSupplier styles

        charAttrs i =
            let
                (isSelected, isCaretLeft) =
                    if focused then
                        selRange
                            |> Maybe.map
                                (\r ->
                                    ( Range.contains (from + i) r
                                    , blinkDisplayCaret && Range.isCaret (from + i) r
                                    )
                                )
                            |> Maybe.withDefault
                                ( False
                                , False
                                )
                    else
                        ( False, False )

            in
            (
                [ dataFrom <| from + i
                , style "display" "inline-block"
                , style "position" "relative"
                , custom "mousedown" <|
                    Json.map2
                        (\offsetX w ->
                            { message = lift (MouseDown (from + i) offsetX w)
                            , preventDefault = True
                            , stopPropagation = True
                            }
                        )
                        (Json.at [ "offsetX" ] Json.float)
                        (Json.at [ "target", "clientWidth" ] Json.int)

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
