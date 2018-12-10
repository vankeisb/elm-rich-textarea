module Textarea exposing
    ( InitData
    , Model
    , Msg
    , attributedRenderer
    , init
    , subscriptions
    , update
    , view
    , HighlightId
    , OutMsg(..)
    , HighlightRequest
    , applyStyles
    , encodeHighlightRequest
    , HighlightResponse
    , highlightResponseDecoder
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
import Internal.Styles as S exposing (Styles)
import Task
import TextUtil exposing (lineRangeAt, wordRangeAt)
import Time exposing (Posix)


type alias Model s =
    Internal.Textarea.Model s


type alias Msg =
    Internal.Textarea.Msg


type alias HighlightId =
    Internal.Textarea.HighlightId


type alias HighlightRequest =
    { id: HighlightId
    , text: String
    }


type OutMsg
    = RequestHighlight HighlightRequest


type alias InitData =
    { initialText : String
    , idPrefix : String
    , debounceMs: Float
    }



debounceConfig : Float -> Debounce.Config Msg
debounceConfig ms =
    { strategy = Debounce.later ms
    , transform = DebounceMsg
    }


init : InitData -> ( Model s, Cmd Msg )
init initData =
    let
        initialModelData =
            { idPrefix = initData.idPrefix
            , text = initData.initialText
            , selection = Nothing
            , styles = S.empty
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
            , highlightId = initialHighlightId
            , debounce = Debounce.init
            , debounceMs = initData.debounceMs
            }
    in
    ( Model initialModelData
        |> applyStyles initialHighlightId []
        |> computeStyledTexts
    , triggerHighlightNow
    )
        |> getViewportPos


requestHighlight: (Model m, Cmd Msg) -> (Model m, Cmd Msg, Maybe OutMsg)
requestHighlight (Model model, cmd) =
    let
        (debounce, debounceCmd) =
            Debounce.push (debounceConfig model.debounceMs) model.highlightId model.debounce
    in
    ( Model
        { model
            | debounce = debounce
        }
    , Cmd.batch
        [ cmd
        , debounceCmd
        ]
    )
        |> noOut


withOutMsg: Maybe OutMsg -> (Model s, Cmd Msg) -> (Model s, Cmd Msg, Maybe OutMsg)
withOutMsg outMsg (model, cmd) =
    (model, cmd, outMsg)


noOut: (Model s, Cmd Msg) -> (Model s, Cmd Msg, Maybe OutMsg)
noOut =
    withOutMsg Nothing



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



view : (Msg -> m) -> Renderer s m -> Model s -> Html m
view lift renderer (Model d) =
    let
        lines =
            d.styledTexts
                |> List.indexedMap
                    (\lineNumber lineElems ->
                        div
                            [ style "display" "flex"
                            , mouseEvent "mousedown" (\_ -> lift <| MouseDownLine lineNumber)
                            , mouseEvent "mouseover" (\_ -> lift <| MouseOverLine lineNumber)
                            , mouseEvent "mouseup" (\_ -> lift <| MouseUpLine lineNumber)
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
            , mouseEvent "mousedown" (\_ -> lift <| BackgroundMouseDown)
            , mouseEvent "mouseover" (\_ -> lift <| BackgroundMouseOver)
            , mouseEvent "mouseup" (\_ -> lift <| BackgroundMouseUp)
            , mouseEvent "mouseleave" (\_ -> lift <| BackgroundMouseLeft)
            , mouseEnterEvent (\buttons -> lift <| BackgroundMouseEnter buttons)
            , on "scroll" <|
                Json.map2
                    (\left top ->
                        lift <| Scrolled left top
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
        , Html.map lift <|
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



computeStyledTexts : Model s -> Model s
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
                                ++ [ S.applyToText
                                        lineWithLf
                                        offset
                                        d.styles
                                   ]
                            )
                        )
                        ( 0, [] )
                    |> Tuple.second
                    |> Debug.log "styledTexts"
        }


noCmd m =
    ( m, Cmd.none )


setSelection : Maybe Range -> ( Model s, Cmd Msg ) -> ( Model s, Cmd Msg )
setSelection r ( Model d, c ) =
    ( Model
        { d
            | selection =
                r
        }
    , c
    )
        |> scrollCaretIntoView d.selection


updateIfSelecting : (Model s -> Model s) -> ( Model s, Cmd Msg) -> ( Model s, Cmd Msg )
updateIfSelecting fun ( Model model, c ) =
    (
        if model.selectingAt /= Nothing then
            ( Model model, c )
                |> Tuple.mapFirst fun
                |> scrollCaretIntoView model.selection

        else
            ( Model model, c )
    )



update : Msg -> Model s -> ( Model s, Cmd Msg, Maybe OutMsg )
update msg (Model model) =
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
                            S.insertAt start inserted model.styles

                        else
                            model.styles
                }
                |> computeStyledTexts
                |> noCmd
                |> setSelection (Just (Range.range start end))
                |> requestHighlight

        OnKeyDown keyCode start end ->
            onKey True keyCode start end model


        OnKeyUp keyCode start end ->
            onKey False keyCode start end model

        MouseDown i ->
            setCaretPos i (Model model)
                |> noOut

        MouseUp i ->
            Model model
                |> noCmd
                |> updateIfSelecting (expandSelection i >> setSelectingAt Nothing)
                |> noOut

        MouseOver i ->
            Model model
                |> noCmd
                |> updateIfSelecting (expandSelection i)
                |> noOut


        MouseClicks i count ->
            if count == 1.0 then
                Model model
                    -- TODO simplify?
                    |> setCaretPos i
                    |> updateIfSelecting (expandSelection i >> setSelectingAt Nothing)
                    |> noOut


            else if count == 2.0 then
                Model model
                    |> expandWordSelection i
                    |> noCmd
                    |> noOut

            else if count == 3.0 then
                Model model
                    |> expandLineSelection i
                    |> noCmd
                    |> noOut

            else
                Model model
                    |> noCmd
                    |> noOut

        MouseOverLine n ->
            Model model
                |> noCmd
                |> updateIfSelecting
                    (\(Model m) ->
                        lineSize n m.text
                            |> Maybe.map (\s -> Model m |> expandSelection (s - 1))
                            |> Maybe.withDefault (Model m)
                    )
                |> noOut


        MouseUpLine n ->
            Model model
                |> noCmd
                |> updateIfSelecting
                    (\(Model m) ->
                        lineSize n m.text
                            |> Maybe.map (\s -> Model m |> expandSelection (s - 1))
                            |> Maybe.map (setSelectingAt Nothing)
                            |> Maybe.withDefault (Model m)
                    )
                |> noOut


        MouseDownLine lineIndex ->
            -- place caret at the end of the line
            lineSize lineIndex model.text
                |> Maybe.map
                    (\s ->
                        setCaretPos (s - 1) (Model model)
                    )
                |> Maybe.withDefault
                    (noCmd <| Model model)
                |> noOut

        BackgroundMouseDown ->
            -- place caret at the end of the text
            setCaretPos
                (String.length model.text)
                (Model model)
                |> noOut


        BackgroundMouseOver ->
            Model model
                |> noCmd
                |> updateIfSelecting (expandSelection <| String.length model.text)
                |> noOut

        BackgroundMouseUp ->
            Model model
                |> noCmd
                |> updateIfSelecting (expandSelection (String.length model.text) >> setSelectingAt Nothing)
                |> noOut


        BackgroundMouseLeft ->
            Model model
                |> setSelectingAt Nothing
                |> noCmd
                |> noOut

        BackgroundMouseEnter buttons ->
            if 1 == buttons then
                Model model
                    |> setSelectingAt (Just 0)
                    |> noCmd
                    |> noOut
            else
               Model model
                    |> noCmd
                    |> noOut

        Focused (Ok ()) ->
            Model
                { model
                    | focused = True
                }
                |> noCmd
                |> noOut

        Focused (Err _) ->
            Model model
                |> noCmd
                |> noOut


        Blurred ->
            Model
                { model
                    | focused = False
                }
                |> setSelectingAt Nothing
                |> noCmd
                |> setSelection Nothing
                |> noOut


        GetViewportPos element ->
            case element of
                Ok e ->
                    let
                        box =
                            model.viewportBox
                    in
                    Model
                        { model
                            | viewportBox =
                                { box
                                    | x = e.element.x
                                    , y = e.element.y
                                }
                        }
                        |> noCmd
                        |> getViewportSize
                        |> noOut

                Err _ ->
                    Model model
                        |> noCmd
                        |> noOut

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
                |> noOut

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
                        |> noOut

                Err _ ->
                    ( Model model, Cmd.none )
                        |> noOut


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
                |> noOut


        NoOp ->
            ( Model model, Cmd.none )
                |> noOut

        DebounceMsg m ->
            let
                (debounce, cmd) =
                    Debounce.update
                        (debounceConfig model.debounceMs)
                        (Debounce.takeLast
                            (\_ -> triggerHighlightNow )
                        )
                        m
                        model.debounce
            in
            ( Model
                { model
                    | debounce =
                        debounce
                }
            , cmd
            )
                |> noOut


        TriggerHighlight ->
            Model model
                |> noCmd
                |> withOutMsg
                        ( Just <|
                            RequestHighlight
                                { id = model.highlightId
                                , text = model.text
                                }
                        )


triggerHighlightNow: Cmd Msg
triggerHighlightNow =
    Task.succeed ()
        |> Task.perform (\_ -> TriggerHighlight)


setCaretPos : Int -> Model s -> ( Model s, Cmd Msg)
setCaretPos i (Model d) =
    ( Model d
        |> setSelectingAt (Just i)
    , focusTextarea d
    )


setSelectingAt : Maybe Int -> Model s -> Model s
setSelectingAt at (Model d) =
    Model { d | selectingAt = at }


expandSelection : Int -> Model s -> Model s
expandSelection to (Model d) =
    Model { d | selection = Maybe.map (Range.expand to) d.selectingAt }


expanSelectionWith : (Int -> String -> Maybe Range) -> Int -> Model s -> Model s
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


expandWordSelection : Int -> Model s -> Model s
expandWordSelection =
    expanSelectionWith wordRangeAt


expandLineSelection : Int -> Model s -> Model s
expandLineSelection =
    expanSelectionWith lineRangeAt


onKey : Bool -> Int -> Int -> Int -> ModelData s -> ( Model s, Cmd Msg, Maybe OutMsg )
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
        |> computeStyledTexts
        |> noCmd
        |> getViewportPos
        |> setSelection newSel
        |> requestHighlight


getViewportPos : ( Model s, Cmd Msg) -> ( Model s, Cmd Msg)
getViewportPos ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getElement (viewportId d)
            |> Task.attempt GetViewportPos
        ]
    )


getViewportSize : ( Model s, Cmd Msg ) -> ( Model s, Cmd Msg )
getViewportSize ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getViewportOf (viewportId d)
            |> Task.attempt GetViewport
        ]
    )


scrollCaretIntoView : Maybe Range -> ( Model s, Cmd Msg ) -> ( Model s, Cmd Msg )
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


subscriptions : Model s -> Sub Msg
subscriptions (Model model) =
    Sub.none


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


attributedRenderer : Model s -> (Msg -> m) -> (List s -> List (Html.Attribute m)) -> Renderer s m
attributedRenderer (Model m) lift attrsSupplier isPrefix str from selRange styles =
    let
        dataFrom f =
            attribute "data-from" <| String.fromInt f

        attrs =
            attrsSupplier styles

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
              , mouseEvent "mousedown" (\adjust -> lift <| MouseDown <| from + i + adjust)
              , mouseEvent "mouseover" (\adjust -> lift <| MouseOver <| from + i + adjust)
              , mouseEvent "mouseup" (\adjust -> lift <| MouseUp <| from + i + adjust)
              , mouseClickEvent (\adjust count -> lift <| MouseClicks (from + i + adjust) count)
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


applyStyles: HighlightId -> List (Range, s) -> Model s -> Model s
applyStyles highlightId styles (Model model) =
    if model.highlightId == highlightId then
        Model
            { model
                | styles =
                    S.empty
                        |> S.addStyles (Debug.log "styles" styles)
            }
            |> computeStyledTexts
    else
        Model model



{-
    Support for ports : encoding and decoding of highlight requests and responses...
-}


encodeHighlightRequest: HighlightRequest -> Encode.Value
encodeHighlightRequest r =
    Encode.object
        [ ("id", encodeHighlightId r.id)
        , ("text", Encode.string r.text)
        ]


type alias HighlightResponse s =
    { id: HighlightId
    , styles : List (Range, s)
    }


highlightResponseDecoder : Json.Decoder s -> Json.Decoder (HighlightResponse s)
highlightResponseDecoder styleDecoder =
    let
        rangeAndStyleDecoder: Json.Decoder (Range, s)
        rangeAndStyleDecoder =
            Json.map2
                (\range styleValue ->
                    (range, styleValue)
                )
                (Json.field "range" rangeDecoder)
                (Json.field "style" styleDecoder)

    in
    Json.map2 HighlightResponse
        (Json.field "id" highlightIdDecoder)
        (Json.field "styles" <|
            Json.list rangeAndStyleDecoder
        )


rangeDecoder: Json.Decoder Range
rangeDecoder =
    Json.map2 Range.range
        (Json.field "from" Json.int)
        (Json.field "to" Json.int)

