module Textarea exposing
    ( Highlighter
    , InitData
    , Model
    , Msg
    , UpdateData
    , attributedRenderer
    , highlight
    , init
    , subscriptions
    , update
    , view
    )

import Array
import Browser
import Browser.Dom as Dom
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
import Time exposing (Posix)


type alias Model s =
    Internal.Textarea.Model s


type alias Msg s =
    Internal.Textarea.Msg s


type alias InitData s =
    { highlighter : Highlighter s
    , initialText : String
    , idPrefix : String
    }


init : InitData s -> ( Model s, Cmd (Msg s) )
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
            }
    in
    ( Model initialModelData
        |> computeStyles initData.highlighter
    , Cmd.none
    )
        |> getViewportPos


focusTextarea : ModelData s -> Cmd (Msg s)
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


type alias Renderer s m =
    String -> String -> Int -> Maybe Range -> List s -> Html m



-- used to display the textarea


devMode =
    False


view : (Msg s -> m) -> Renderer s m -> Model s -> Html m
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
            , id <| viewportId d
            , mouseEvent "mousedown" (\_ -> lift <| BackgroundClicked)
            , mouseEvent "mouseover" (\_ -> lift <| BackgroundMouseOver)
            , mouseEvent "mouseup" (\_ -> lift <| BackgroundMouseUp)
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


type alias Highlighter s =
    String -> List ( Range, s )


computeStylesSync : Highlighter s -> Model s -> ( Model s, Cmd (Msg s) )
computeStylesSync highlighter model =
    ( computeStyles highlighter model, Cmd.none )


computeStylesAsync2 : UpdateData m s -> ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd m )
computeStylesAsync2 updateData ( Model d, cmd ) =
    let
        model1 =
            { d | highlightId = d.highlightId + 1 }
    in
    ( computeStyledTexts <| Model model1
    , Cmd.batch
        [ Cmd.map updateData.lift cmd
        , updateData.onHighlight ( model1.text, model1.highlightId )
        ]
    )


liftCmd : UpdateData m s -> ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd m )
liftCmd updateData ( model, cmd ) =
    ( model, Cmd.map updateData.lift cmd )


computeStyles : Highlighter s -> Model s -> Model s
computeStyles highlighter (Model d) =
    Model d
        |> updateStyles (highlighter d.text)


updateStyles : List ( Range, s ) -> Model s -> Model s
updateStyles styles (Model d) =
    Model
        { d
            | styles =
                Styles.empty
                    |> Styles.addStyles styles
        }
        |> computeStyledTexts


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


setSelection : Maybe Range -> ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd (Msg s) )
setSelection r ( Model d, c ) =
    ( Model
        { d
            | selection =
                r
        }
    , c
    )
        |> scrollCaretIntoView d.selection


updateIfSelecting : (Model s -> Model s) -> ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd (Msg s) )
updateIfSelecting fun ( Model model, c ) =
    if model.selectingAt /= Nothing then
        ( Model model, c )
            |> Tuple.mapFirst fun
            |> scrollCaretIntoView model.selection

    else
        ( Model model, c )


highlight : List ( Range, s ) -> Int -> Model s -> ( Model s, Cmd (Msg s) )
highlight styles highlightId (Model model) =
    if model.highlightId == highlightId then
        ( updateStyles styles <| Model model, Cmd.none )

    else
        ( Model model, Cmd.none )


type alias UpdateData m s =
    { onHighlight : ( String, Int ) -> Cmd m
    , lift : Msg s -> m
    }


update : UpdateData m s -> Msg s -> Model s -> ( Model s, Cmd m )
update updateData msg (Model model) =
    case msg of
        OnInput s start end ->
            Model
                { model
                    | text = s
                    , styles =
                        if start == end && String.length model.text < String.length s then
                            Styles.insertAt start model.styles

                        else
                            model.styles
                }
                |> noCmd
                |> setSelection (Just (Range.range start end))
                |> computeStylesAsync2 updateData

        OnKeyDown keyCode start end ->
            onKey True keyCode start end model
                |> computeStylesAsync2 updateData

        OnKeyUp keyCode start end ->
            onKey False keyCode start end model
                |> computeStylesAsync2 updateData

        MouseDown i ->
            setCaretPos i (Model model)
                |> liftCmd updateData

        MouseUp i ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection i >> setSelectingAt Nothing)
                |> liftCmd updateData

        MouseOver i ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection i)
                |> liftCmd updateData

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
                |> liftCmd updateData

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
                |> liftCmd updateData

        BackgroundClicked ->
            -- place caret at the end of the text
            setCaretPos
                (String.length model.text)
                (Model model)
                |> liftCmd updateData

        BackgroundMouseOver ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection <| String.length model.text)
                |> liftCmd updateData

        BackgroundMouseUp ->
            ( Model model
            , Cmd.none
            )
                |> updateIfSelecting (expandSelection (String.length model.text) >> setSelectingAt Nothing)
                |> liftCmd updateData

        MouseDownLine lineIndex ->
            -- place caret at the end of the line
            lineSize lineIndex model.text
                |> Maybe.map
                    (\s ->
                        setCaretPos (s - 1) (Model model)
                    )
                |> Maybe.withDefault
                    ( Model model, Cmd.none )
                |> liftCmd updateData

        Focused (Ok ()) ->
            ( Model
                { model
                    | focused = True
                }
            , Cmd.none
            )
                |> liftCmd updateData

        Focused (Err _) ->
            ( Model model, Cmd.none )
                |> liftCmd updateData

        Blurred ->
            ( Model
                { model
                    | focused = False
                }
                |> setSelectingAt Nothing
            , Cmd.none
            )
                |> liftCmd updateData

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
                        |> liftCmd updateData

                Err _ ->
                    ( Model model, Cmd.none )
                        |> liftCmd updateData

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
                |> liftCmd updateData

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
                        |> liftCmd updateData

                Err _ ->
                    ( Model model, Cmd.none )
                        |> liftCmd updateData

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
                |> liftCmd updateData

        NoOp ->
            ( Model model, Cmd.none )
                |> liftCmd updateData


setCaretPos : Int -> Model s -> ( Model s, Cmd (Msg s) )
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


onKey : Bool -> Int -> Int -> Int -> ModelData s -> ( Model s, Cmd (Msg s) )
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


getViewportPos : ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd (Msg s) )
getViewportPos ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getElement (viewportId d)
            |> Task.attempt GetViewportPos
        ]
    )


getViewportSize : ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd (Msg s) )
getViewportSize ( Model d, c ) =
    ( Model d
    , Cmd.batch
        [ c
        , Dom.getViewportOf (viewportId d)
            |> Task.attempt GetViewport
        ]
    )


scrollCaretIntoView : Maybe Range -> ( Model s, Cmd (Msg s) ) -> ( Model s, Cmd (Msg s) )
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


subscriptions : Model s -> Sub (Msg s)
subscriptions (Model model) =
    Sub.none


addStyles : List ( Range, s ) -> Model s -> Model s
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



-- place caret at index i or i+1, depending
-- on the location of the click inside the
-- char wrapper


adjustIndex : Float -> Int -> Int
adjustIndex offsetX clientWidth =
    if offsetX >= (toFloat clientWidth * 0.5) then
        1

    else
        0


attributedRenderer : Model s -> (Msg s -> m) -> (List s -> List (Html.Attribute m)) -> Renderer s m
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
