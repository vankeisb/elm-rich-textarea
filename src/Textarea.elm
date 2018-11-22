module Textarea exposing
    ( Model
    , Msg
    , init
    , view
    , update
    , subscriptions
    , attributedRenderer
    , InitData
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


type alias Dimensions =
    { h: Float
    , w: Float
    }


type alias ModelData s =
    { idPrefix: String
    , text: String
    , selection: Maybe Range
    , styles: Styles s
    , styledTexts: List (List (StyledText s))
    , focused: Bool
    , time: Posix
    , blinkStart: Posix
    , viewportBox: Box
    }


type Model s =
    Model (ModelData s)


type alias Box =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    , scrollTop: Float
    , scrollLeft: Float
    }


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
    | GetViewportPos (Result Dom.Error Dom.Element)
    | GetViewport (Result Dom.Error Dom.Viewport)
    | GetCharViewport (Result Dom.Error Dom.Element)
    | Scrolled Float Float
    | NoOp



type alias InitData s =
    { highlighter: Highlighter s
    , initialText: String
    , idPrefix: String
    }


init : InitData s -> (Model s, Cmd Msg)
init initData =
    let
        initialModelData =
            { idPrefix = initData.idPrefix
            , text = initData.initialText
            , selection = Nothing
            , styles = Styles.empty
            , styledTexts = []
            , focused = False
            , time = Time.millisToPosix 0
            , blinkStart = Time.millisToPosix 0
            , viewportBox =
                { h = 0
                , w = 0
                , x = 0
                , y = 0
                , scrollTop = 0
                , scrollLeft = 0
                }
            }
    in
    ( Model initialModelData
        |> computeStyles initData.highlighter
    , Cmd.none
    )
        |> getViewportPos


focusTextarea : ModelData s -> Cmd Msg
focusTextarea d =
    Dom.focus (textareaId d)
            |> Task.attempt Focused


textareaId : ModelData s -> String
textareaId d =
    d.idPrefix ++ "-textarea"



viewportId : ModelData s -> String
viewportId d =
    d.idPrefix ++ "-viewport"


charId : ModelData s -> Int -> String
charId d i =
    d.idPrefix ++ "-char-" ++ (String.fromInt i)


{-
    Applies styles to a string at a given offset. Selection
    range is also passed for drawing the selection.
-}
type alias  Renderer s m = String -> String -> Int -> Maybe Range -> Bool -> Bool -> List s -> Html m


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
                                            d.idPrefix
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
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        ]
        [ div
            [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "right" "0"
            , style "bottom" "0"
            , style "white-space" "pre"
            , style "overflow" "auto"
            , id <| viewportId d
            , custom "mousedown" <|
                Json.succeed
                    { message = lift BackgroundClicked
                    , preventDefault = True
                    , stopPropagation = True
                    }
            , on "scroll" <|
                Json.map2
                    (\left top ->
                        lift <| Scrolled left top
                    )
                    (Json.at [ "target", "scrollLeft" ] Json.float)
                    (Json.at [ "target", "scrollTop" ] Json.float)
            ]
            lines
        , Html.map lift <|
            textarea
                [ value d.text
                , id <| textareaId d
                , style "position" "fixed"
                , style "left" "-10000px"
                , style "top" "-10000px"
--                , style "width" "400px"
--                , style "height" "200px"
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

        GetViewportPos element ->
            case element of
                Ok e ->
                    let
                        box =
                            model.viewportBox
                    in
                    ( Model
                        { model
                            | viewportBox =
                                { box
                                    | x = e.element.x
                                    , y = e.element.y
                                }
                        }
                    , Cmd.none
                    )
                        |> getViewportSize

                Err _ ->
                    (Model model, Cmd.none)



        GetViewport vp ->
            ( case vp of
                Ok v ->
                    let
                        box =
                            model.viewportBox
                    in
                    Model
                        { model
                            | viewportBox =
                                { box
                                    | h =
                                        v.viewport.height
                                    , w =
                                        v.viewport.width
                                }
                        }
                Err _ ->
                    Model model
            , Cmd.none
            )

        GetCharViewport element ->
            case element of
                Ok e ->
                    let
                        scrollTop =
                            model.viewportBox.scrollTop
                                |> Debug.log "scrollTop"

                        charTop =
                            e.element.y

                        charBottom =
                            charTop + e.element.height

                        viewportTop =
                            model.viewportBox.y

                        topDelta =
                            viewportTop - charTop
                                |> Debug.log "topDelta"

                        viewportBottom =
                            viewportTop + model.viewportBox.h

                        bottomDelta =
                            charBottom - viewportBottom

                        scrollLeft =
                            model.viewportBox.scrollLeft

                        setScroll x y =
                            Dom.setViewportOf
                                (viewportId model)
                                x
                                y
                                |> Task.attempt
                                    (\_ -> NoOp)

                        scrollV =
                            if bottomDelta > 0 then
                                setScroll scrollLeft (scrollTop + bottomDelta)
                            else if topDelta > 0 then
                                setScroll scrollLeft (scrollTop - topDelta)
                            else
                                Cmd.none
                    in
                    ( Model model
                    , scrollV
                    )
                Err _ ->
                    (Model model, Cmd.none)

        Scrolled x y ->
            let
                box =
                    model.viewportBox

                newBox =
                    { box
                        | scrollTop = y
                    }
            in
            ( Model
                { model
                    | viewportBox =
                        newBox
                }
            , Cmd.none
            )

        NoOp ->
            (Model model, Cmd.none)


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
    , focusTextarea d
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

        newData =
            { d
                | selection =
                    newSel
                , text =
                    newText
            }

        res =
            Model newData
                |> computeStyles hl
                |> triggerBlink
                |> getViewportPos
    in
    if not isDown then
        getCaretPos res
    else
        res



getViewportPos : (Model s, Cmd Msg) -> (Model s, Cmd Msg)
getViewportPos (Model d, c) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getElement (viewportId d)
            |> Task.attempt GetViewportPos
        ]
    )



getViewportSize: (Model s, Cmd Msg) -> (Model s, Cmd Msg)
getViewportSize (Model d, c) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getViewportOf (viewportId d)
            |> Task.attempt GetViewport
        ]
    )



getCaretPos: (Model s, Cmd Msg) -> (Model s, Cmd Msg)
getCaretPos (Model d, c) =
    let
        cmd =
            d.selection
                |> Maybe.map
                    (\r ->
                        if Range.isCaret (Range.getFrom r) r then
                            Dom.getElement
                                (charId d (Range.getFrom r))
                                |> Task.attempt GetCharViewport
                        else
                            Cmd.none
                    )
                |> Maybe.withDefault Cmd.none

    in
    (Model d
    , Cmd.batch
        [ c
        , cmd
        ]
    )



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
        Sub.none
--        Time.every 100 OnTime
    else
        Sub.none



addStyles : List (Range, s) -> Model s -> Model s
addStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.addStyles styles d.styles
        }



attributedRenderer : Model s -> (Msg -> m) -> (List s -> List (Html.Attribute m)) -> Renderer s m
attributedRenderer (Model m) lift attrsSupplier isPrefix str from selRange focused blinkDisplayCaret styles =
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
                , id <| charId m (from + i)
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
