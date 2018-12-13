module Textarea exposing
    ( Highlighter
    , InitData
    , Model
    , Msg
    , ReturnStyles
    , attributedRenderer
    , init
    , subscriptions
    , update
    , view
    )

import Array
import Browser
import Browser.Dom as Dom
import Debounce
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Internal.Textarea exposing (..)
import Json.Decode as Json
import Json.Encode as Encode
import Process
import Range exposing (Range)
import Styles exposing (Styles)
import Task
import TextUtil exposing (lineRangeAt, wordRangeAt)
import Time exposing (Posix)


type alias Model msg s =
    Internal.Textarea.Model msg s


type alias Msg s =
    Internal.Textarea.Msg s


type alias ReturnStyles msg s =
    Internal.Textarea.ReturnStyles msg s


type alias InitData msg s =
    { initialText : String
    , idPrefix : String
    , lift : Msg s -> msg
    , resolveStyles : StyleResolver msg s
    }


type alias OnHighlight msg s =
    ReturnStyles msg s -> String -> Cmd msg


init : InitData msg s -> ( Model msg s, Cmd msg )
init initData =
    let
        initialModelData =
            { idPrefix = initData.idPrefix
            , text = initData.initialText
            , selection = Nothing
            , styles = Styles.empty
            , styledTexts = []
            , focused = False
            , viewportBox =
                { h = 0
                , w = 0
                , x = 0
                , y = 0
                , scrollTop = 0
                , scrollLeft = 0
                }
            , selectingAt = Nothing
            , highlightId = 0
            , debounce = Debounce.init
            , lift = initData.lift
            , resolveStyles = initData.resolveStyles
            }
    in
    ( Model initialModelData
    , Cmd.none
    )
        |> getViewportPos
        |> computeStylesAsync


focusTextarea : ModelData msg s -> Cmd (Msg s)
focusTextarea d =
    Dom.focus (textareaId d)
        |> Task.attempt Focused


textareaId : ModelData msg s -> String
textareaId d =
    d.idPrefix ++ "-textarea"


viewportId : ModelData msg s -> String
viewportId d =
    d.idPrefix ++ "-viewport"


charId : ModelData msg s -> Int -> String
charId d i =
    d.idPrefix ++ "-char-" ++ String.fromInt i



{-
   Applies styles to a string at a given offset. Selection
   range is also passed for drawing the selection.
-}
-- TODO simplify


type alias Renderer s m =
    String -> String -> Int -> Maybe Range -> List s -> Html m



-- used to display the textarea


devMode =
    False


view : Model msg s -> Html msg
view (Model d) =
    let
        renderer =
            attributedRenderer (Model d)

        lines =
            d.styledTexts
                |> List.indexedMap
                    (\lineNumber lineElems ->
                        div
                            [ style "display" "flex"
                            , mouseEvent "mousedown" (\_ -> d.lift <| MouseDownLine lineNumber)
                            , mouseEvent "mouseover" (\_ -> d.lift <| MouseOverLine lineNumber)
                            , mouseEvent "mouseup" (\_ -> d.lift <| MouseUpLine lineNumber)
                            ]
                            (lineElems
                                |> List.map
                                    (\e ->
                                        renderer
                                            d.idPrefix
                                            e.text
                                            (Range.getFrom e.range)
                                            d.selection
                                            e.styles
                                    )
                            )
                    )

        ( ss, se ) =
            d.selection
                |> Maybe.map Range.getBounds
                |> Maybe.withDefault ( 0, 0 )
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
            , style "user-select" "none"
            , id <| viewportId d
            , mouseEvent "mousedown" (\_ -> d.lift <| BackgroundMouseDown)
            , mouseEvent "mouseover" (\_ -> d.lift <| BackgroundMouseOver)
            , mouseEvent "mouseup" (\_ -> d.lift <| BackgroundMouseUp)
            , mouseEvent "mouseleave" (\_ -> d.lift <| BackgroundMouseLeft)
            , mouseEnterEvent (\buttons -> d.lift <| BackgroundMouseEnter buttons)
            , on "scroll" <|
                Json.map2
                    (\left top ->
                        d.lift <| Scrolled left top
                    )
                    (Json.at [ "target", "scrollLeft" ] Json.float)
                    (Json.at [ "target", "scrollTop" ] Json.float)
            ]
            lines
        , node "style"
            [ attribute "scoped" ""
            ]
            [ text """
                    .blinking-cursor {
                        opacity: 1;
                        animation: 1s blink step-end infinite;
                    }

                    @keyframes blink {
                      from, to {
                        opacity: 1;
                      }
                      50% {
                        opacity: 0;
                      }
                    }
        """
            ]
        , Html.map d.lift <|
            textarea
                [ value d.text
                , id <| textareaId d
                , style "position" "fixed"
                , style "padding" "0"
                , style "left" <|
                    if devMode then
                        "350px"

                    else
                        "-10000px"
                , style "top" <|
                    if devMode then
                        "65px"

                    else
                        "-10000px"
                , style "width" "200px"
                , style "height" "100px"
                , property "selectionStart" <| Encode.int ss
                , property "selectionEnd" <| Encode.int se
                , style "white-space" "nowrap"
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
                                keyCode == 9

                            -- stop tab
                            , stopPropagation =
                                keyCode == 9

                            -- stop tab
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
                                keyCode == 9

                            -- stop tab
                            , stopPropagation =
                                keyCode == 9

                            -- stop tab
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


type alias Highlighter s =
    String -> List ( Range, s )


computeStylesAsync : ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd msg )
computeStylesAsync ( Model model, cmd ) =
    let
        ( debounce, cmd1 ) =
            Debounce.push debounceConfig model.text model.debounce
    in
    ( computeStyledTexts <| Model { model | debounce = debounce }
    , Cmd.batch
        [ cmd
        , cmd1
        ]
        |> Cmd.map model.lift
    )


liftCmd : ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd msg )
liftCmd ( Model model, cmd ) =
    ( Model model, Cmd.map model.lift cmd )


computeStyles : Highlighter s -> Model msg s -> Model msg s
computeStyles highlighter (Model d) =
    Model d
        |> updateStyles (highlighter d.text)


updateStyles : List ( Range, s ) -> Model msg s -> Model msg s
updateStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.empty
                    |> Styles.addStyles styles
        }
        |> computeStyledTexts


computeStyledTexts : Model msg s -> Model msg s
computeStyledTexts (Model d) =
    Model
        { d
            | styledTexts =
                String.split "\n" d.text
                    |> List.foldl
                        (\line ( offset, res ) ->
                            let
                                lineWithLf =
                                    line ++ "\n"
                            in
                            ( offset + String.length lineWithLf
                            , res
                                ++ [ Styles.applyToText
                                        lineWithLf
                                        offset
                                        d.styles
                                   ]
                            )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


noCmd m =
    ( m, Cmd.none )


setSelection : Maybe Range -> ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd (Msg s) )
setSelection r ( Model d, c ) =
    ( Model
        { d
            | selection =
                r
        }
    , c
    )
        |> scrollCaretIntoView d.selection


updateIfSelecting : (Model msg s -> Model msg s) -> ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd (Msg s) )
updateIfSelecting fun ( Model model, c ) =
    if model.selectingAt /= Nothing then
        ( Model model, c )
            |> Tuple.mapFirst fun
            |> scrollCaretIntoView model.selection

    else
        ( Model model, c )


update : OnHighlight msg s -> Msg s -> Model msg s -> ( Model msg s, Cmd msg )
update onHighlight msg (Model model) =
    case msg of
        OnInput s start end ->
            Model
                { model
                    | text = s
                    , styles =
                        let
                            inserted =
                                String.length s - String.length model.text
                        in
                        if start == end && inserted /= 0 then
                            Styles.insertAt start inserted model.styles

                        else
                            model.styles
                }
                |> noCmd
                |> setSelection (Just (Range.range start end))
                |> computeStylesAsync

        OnKeyDown keyCode start end ->
            onKey True keyCode start end model
                |> computeStylesAsync

        OnKeyUp keyCode start end ->
            onKey False keyCode start end model
                |> computeStylesAsync

        MouseDown i ->
            setCaretPos i (Model model)
                |> liftCmd

        MouseUp i ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection i >> setSelectingAt Nothing)
                |> liftCmd

        MouseOver i ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection i)
                |> liftCmd

        MouseClicks i count ->
            if count == 1.0 then
                Model model
                    -- TODO simplify?
                    |> setCaretPos i
                    |> updateIfSelecting (expandSelection i >> setSelectingAt Nothing)
                    |> liftCmd

            else if count == 2.0 then
                ( Model model
                    |> expandWordSelection i
                , Cmd.none
                )
                    |> liftCmd

            else if count == 3.0 then
                ( Model model
                    |> expandLineSelection i
                , Cmd.none
                )
                    |> liftCmd

            else
                ( Model model
                , Cmd.none
                )

        MouseOverLine n ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting
                    (\(Model m) ->
                        lineSize n m.text
                            |> Maybe.map (\s -> Model m |> expandSelection (s - 1))
                            |> Maybe.withDefault (Model m)
                    )
                |> liftCmd

        MouseUpLine n ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting
                    (\(Model m) ->
                        lineSize n m.text
                            |> Maybe.map (\s -> Model m |> expandSelection (s - 1))
                            |> Maybe.map (setSelectingAt Nothing)
                            |> Maybe.withDefault (Model m)
                    )
                |> liftCmd

        MouseDownLine lineIndex ->
            -- place caret at the end of the line
            lineSize lineIndex model.text
                |> Maybe.map
                    (\s ->
                        setCaretPos (s - 1) (Model model)
                    )
                |> Maybe.withDefault
                    ( Model model, Cmd.none )
                |> liftCmd

        BackgroundMouseDown ->
            -- place caret at the end of the text
            setCaretPos
                (String.length model.text)
                (Model model)
                |> liftCmd

        BackgroundMouseOver ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection <| String.length model.text)
                |> liftCmd

        BackgroundMouseUp ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection (String.length model.text) >> setSelectingAt Nothing)
                |> liftCmd

        BackgroundMouseLeft ->
            ( Model model |> setSelectingAt Nothing
            , Cmd.none
            )
                |> liftCmd

        BackgroundMouseEnter buttons ->
            if 1 == buttons then
                ( Model model |> setSelectingAt (Just 0)
                , Cmd.none
                )
                    |> liftCmd

            else
                ( Model model
                , Cmd.none
                )

        Focused (Ok ()) ->
            ( Model
                { model
                    | focused = True
                }
            , Cmd.none
            )
                |> liftCmd

        Focused (Err _) ->
            ( Model model, Cmd.none )
                |> liftCmd

        Blurred ->
            ( Model
                { model
                    | focused = False
                }
                |> setSelectingAt Nothing
            , Cmd.none
            )
                |> liftCmd

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
                        |> liftCmd

                Err _ ->
                    ( Model model, Cmd.none )
                        |> liftCmd

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
                |> liftCmd

        GetCharViewport element ->
            case element of
                Ok e ->
                    let
                        lead =
                            e.element.height

                        scrollTop =
                            model.viewportBox.scrollTop

                        charTop =
                            e.element.y

                        charBottom =
                            charTop + e.element.height

                        viewportTop =
                            model.viewportBox.y + lead

                        topDelta =
                            viewportTop - charTop

                        viewportBottom =
                            viewportTop + model.viewportBox.h - 2 * lead

                        bottomDelta =
                            charBottom - viewportBottom

                        scrollLeft =
                            model.viewportBox.scrollLeft

                        charLeft =
                            e.element.x

                        charRight =
                            charLeft + e.element.width

                        viewportLeft =
                            model.viewportBox.x + lead

                        leftDelta =
                            viewportLeft - charLeft

                        viewportRight =
                            viewportLeft + model.viewportBox.w - 2 * lead

                        rightDelta =
                            charRight - viewportRight

                        setScroll x y =
                            Dom.setViewportOf
                                (viewportId model)
                                x
                                y
                                |> Task.attempt
                                    (\_ -> NoOp)

                        newScrollTop =
                            if bottomDelta > 0 then
                                scrollTop + bottomDelta

                            else if topDelta > 0 then
                                scrollTop - topDelta

                            else
                                scrollTop

                        newScrollLeft =
                            if rightDelta > 0 then
                                scrollLeft + rightDelta

                            else if leftDelta > 0 then
                                scrollLeft - leftDelta

                            else
                                scrollLeft
                    in
                    ( Model model
                    , setScroll newScrollLeft newScrollTop
                    )
                        |> liftCmd

                Err _ ->
                    ( Model model, Cmd.none )
                        |> liftCmd

        Scrolled x y ->
            let
                box =
                    model.viewportBox

                newBox =
                    { box
                        | scrollTop = y
                        , scrollLeft = x
                    }
            in
            ( Model
                { model
                    | viewportBox =
                        newBox
                }
            , Cmd.none
            )
                |> liftCmd

        NoOp ->
            ( Model model, Cmd.none )
                |> liftCmd

        RequestHighlight text ->
            let
                model1 =
                    { model | highlightId = model.highlightId + 1 }

                return =
                    \styles ->
                        Task.succeed styles
                            |> Task.perform (NewHighlight model1.highlightId)
                            |> Cmd.map model1.lift
            in
            ( Model model1, onHighlight return text )

        NewHighlight highlightId styles ->
            if model.highlightId == highlightId then
                ( updateStyles styles <| Model model, Cmd.none )

            else
                ( Model model, Cmd.none )

        DebounceMsg msg1 ->
            let
                save text =
                    Task.perform RequestHighlight <| Task.succeed text

                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        msg1
                        model.debounce
            in
            ( Model { model | debounce = debounce }
            , cmd
            )
                |> liftCmd


debounceConfig : Debounce.Config (Msg s)
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }


setCaretPos : Int -> Model msg s -> ( Model msg s, Cmd (Msg s) )
setCaretPos i (Model d) =
    ( Model d
        |> setSelectingAt (Just i)
    , focusTextarea d
    )


setSelectingAt : Maybe Int -> Model msg s -> Model msg s
setSelectingAt at (Model d) =
    Model { d | selectingAt = at }


expandSelection : Int -> Model msg s -> Model msg s
expandSelection to (Model d) =
    Model { d | selection = Maybe.map (Range.expand to) d.selectingAt }


expanSelectionWith : (Int -> String -> Maybe Range) -> Int -> Model msg s -> Model msg s
expanSelectionWith fun pos (Model d) =
    let
        selection =
            fun pos d.text
                |> Maybe.withDefault (Range.range pos pos)
    in
    Model
        { d
            | selection = Just selection
        }


expandWordSelection : Int -> Model msg s -> Model msg s
expandWordSelection =
    expanSelectionWith wordRangeAt


expandLineSelection : Int -> Model msg s -> Model msg s
expandLineSelection =
    expanSelectionWith lineRangeAt


onKey : Bool -> Int -> Int -> Int -> ModelData msg s -> ( Model msg s, Cmd (Msg s) )
onKey isDown keyCode start end d =
    let
        ( newText, newSel ) =
            if keyCode == 9 && not isDown then
                -- TAB: insert spaces
                d.selection
                    |> Maybe.map
                        (\r ->
                            let
                                ( from, to ) =
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
            | text = newText
        }
        |> noCmd
        |> getViewportPos
        |> setSelection newSel


getViewportPos : ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd (Msg s) )
getViewportPos ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getElement (viewportId d)
            |> Task.attempt GetViewportPos
        ]
    )


getViewportSize : ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd (Msg s) )
getViewportSize ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getViewportOf (viewportId d)
            |> Task.attempt GetViewport
        ]
    )


scrollCaretIntoView : Maybe Range -> ( Model msg s, Cmd (Msg s) ) -> ( Model msg s, Cmd (Msg s) )
scrollCaretIntoView prevRange ( Model d, c ) =
    let
        scrollCmd charIndex =
            Dom.getElement
                (charId d charIndex)
                |> Task.attempt GetCharViewport

        cmd =
            d.selection
                |> Maybe.map
                    (\r ->
                        let
                            ( newFrom, newTo ) =
                                Range.getBounds r
                        in
                        -- compare new selection with previous one
                        -- and find the "caret" pos
                        if newFrom == newTo then
                            -- new range is a caret : we can get the charId
                            scrollCmd (Range.getFrom r)

                        else
                            -- we need to compare with the previous range and
                            -- see how it "expanded"
                            case prevRange of
                                Just pr ->
                                    let
                                        ( oldFrom, oldTo ) =
                                            Range.getBounds pr
                                    in
                                    if oldFrom == newFrom && oldTo /= newTo then
                                        -- "right" expansion : use the right offset
                                        scrollCmd newTo

                                    else if oldFrom /= newFrom && oldTo == newTo then
                                        -- "left" expansion : use the left offset
                                        scrollCmd newFrom

                                    else
                                        Cmd.none

                                Nothing ->
                                    Cmd.none
                    )
                |> Maybe.withDefault Cmd.none
    in
    ( Model d
    , Cmd.batch
        [ c
        , cmd
        ]
    )


subscriptions : Model msg s -> Sub (Msg s)
subscriptions (Model model) =
    Sub.none


addStyles : List ( Range, s ) -> Model msg s -> Model msg s
addStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.addStyles styles d.styles
        }


mouseEvent : String -> (Int -> m) -> Attribute m
mouseEvent name createMsg =
    custom name <|
        Json.map2
            (\offsetX w ->
                { message = createMsg (adjustIndex offsetX w)
                , preventDefault = True
                , stopPropagation = True
                }
            )
            (Json.at [ "offsetX" ] Json.float)
            (Json.at [ "target", "clientWidth" ] Json.int)


mouseEnterEvent : (Int -> msg) -> Attribute msg
mouseEnterEvent createMsg =
    custom "mouseenter" <|
        Json.map
            (\buttons ->
                { message = createMsg buttons
                , preventDefault = True
                , stopPropagation = True
                }
            )
            (Json.at [ "buttons" ] Json.int)


mouseClickEvent : (Int -> Float -> msg) -> Attribute msg
mouseClickEvent createMsg =
    custom "click" <|
        Json.map3
            (\offsetX w detail ->
                { message = createMsg (adjustIndex offsetX w) detail
                , preventDefault = True
                , stopPropagation = True
                }
            )
            (Json.at [ "offsetX" ] Json.float)
            (Json.at [ "target", "clientWidth" ] Json.int)
            (Json.at [ "detail" ] Json.float)



-- place caret at index i or i+1, depending
-- on the location of the click inside the
-- char wrapper


adjustIndex : Float -> Int -> Int
adjustIndex offsetX clientWidth =
    if offsetX >= (toFloat clientWidth * 0.5) then
        1

    else
        0


attributedRenderer : Model msg s -> Renderer s msg
attributedRenderer (Model m) isPrefix str from selRange styles =
    let
        dataFrom f =
            attribute "data-from" <| String.fromInt f

        attrs =
            m.resolveStyles styles

        charAttrs i =
            let
                ( isSelected, isCaretLeft ) =
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
            ( [ dataFrom <| from + i
              , style "display" "inline-block"
              , style "position" "relative"
              , id <| charId m (from + i)
              , mouseEvent "mousedown" (\adjust -> m.lift <| MouseDown <| from + i + adjust)
              , mouseEvent "mouseover" (\adjust -> m.lift <| MouseOver <| from + i + adjust)
              , mouseEvent "mouseup" (\adjust -> m.lift <| MouseUp <| from + i + adjust)
              , mouseClickEvent (\adjust count -> m.lift <| MouseClicks (from + i + adjust) count)
              ]
                ++ (if isSelected then
                        [ style "background-color" "lightblue" ]

                    else
                        []
                   )
            , isCaretLeft
            )
    in
    span
        attrs
        (str
            |> String.toList
            |> List.indexedMap
                (\i c ->
                    let
                        ( ca, isCaretLeft ) =
                            charAttrs i
                    in
                    div
                        ca
                        [ if isCaretLeft then
                            div
                                [ style "border-left" "1px solid black"
                                , style "position" "absolute"
                                , style "top" "0"
                                , style "left" "0"
                                , style "bottom" "0"
                                , style "width" "0px"
                                , style "box-sizing" "border-box"
                                , class "blinking-cursor"
                                ]
                                []

                          else
                            text ""
                        , text (String.fromChar c)
                        ]
                )
        )
