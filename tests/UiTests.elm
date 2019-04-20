module UiTests exposing
    ( suite
    , updateSuite
    )

import Expect exposing (Expectation)
import Html
import Html.Attributes as A
import Internal.Textarea as IT
import Json.Encode as Encode
import List exposing (indexedMap)
import Range exposing (Range, range)
import String exposing (fromChar, fromInt, toList)
import Task
import Test exposing (..)
import Test.Html.Event as E
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)
import Textarea exposing (..)


type Msg
    = TextareaMsg Textarea.Msg
    | NoOp ( String, Int )


type MyStyle
    = Style1
    | Style2


suite : Test
suite =
    describe "UI Tests"
        [ test "single line, no styles" <|
            \_ ->
                createModel "foo bar baz"
                    |> renderHtml
                    |> Expect.all
                        ("foo bar baz"
                            |> String.toList
                            |> List.indexedMap
                                (\i c ->
                                    \html ->
                                        html
                                            |> find
                                                [ attribute <|
                                                    A.attribute "data-from" (String.fromInt i)
                                                ]
                                            |> children [ text (String.fromChar c) ]
                                            |> count (Expect.equal 1)
                                )
                        )
        , test "click line should trigger LineClicked" <|
            \_ ->
                let
                    simulatedEvent =
                        Encode.object
                            [ ( "offsetX", Encode.float 0 )
                            , ( "target"
                              , Encode.object
                                    [ ( "clientWidth", Encode.int 0 ) ]
                              )
                            ]
                in
                createModel "foo\nbar\nbaz"
                    |> renderHtml
                    |> find
                        [ style "display" "flex"
                        , containing
                            [ attribute <|
                                A.attribute "data-from" "6"
                            , text "r"
                            ]
                        ]
                    |> E.simulate
                        (E.custom "mousedown" simulatedEvent)
                    |> E.expect (TextareaMsg <| IT.MouseDownLine 1)
        , test "do not place caret on mouse down (line)" <|
            \_ ->
                createModel "foo\nbar\nbaz"
                    |> update (IT.MouseDownLine 1)
                    |> Expect.all
                        [ \(IT.Model m) ->
                            Expect.equal
                                Nothing
                                m.selection
                        ]
        , test "when clicking the bg then the caret should not be set" <|
            \_ ->
                createModel "foo\nbar\nbaz"
                    |> update IT.BackgroundMouseDown
                    |> Expect.all
                        [ getSelection >> Expect.equal Nothing
                        , getSelectingAt >> Expect.equal (Just 11)
                        , renderHtml >> hasNot [ hasCaretAt 11 ]
                        ]
        , test "when clicking the bg then the caret should be at the end of text" <|
            \_ ->
                createModel "foo\nbar\nbaz"
                    |> whileSelectingAt 11
                    |> update IT.BackgroundMouseUp
                    |> renderHtml
                    |> has [ hasCaretAt 11 ]
        , test "when mouse down on the line then the caret should be at the end of this line" <|
            \_ ->
                createModel "foo\nbar\nbaz"
                    |> whileSelectingAt 3
                    |> update (IT.MouseUpLine 0)
                    |> renderHtml
                    |> has [ hasCaretAt 3 ]
        , test "when clicking (mouse up and down) on the line then the caret should be at the end of this line" <|
            \_ ->
                createModel "foo\nbar\nbaz"
                    |> withSelection (range 3 3)
                    |> whileSelectingAt 3
                    |> update (IT.MouseUpLine 0)
                    |> renderHtml
                    |> has [ hasCaretAt 3 ]
        , describe "mouse selection"
            [ test "expand to end of line" <|
                \_ ->
                    createModel "gnu\nbar\nbaz"
                        |> withSelection (range 1 2)
                        |> whileSelectingAt 1
                        |> update (IT.MouseOverLine 0)
                        |> renderHtml
                        |> expectSelectedText "nu"
            , test "expand to right" <|
                \_ ->
                    createModel "gnu\nbar\nbaz"
                        |> whileSelectingAt 5
                        |> update (IT.MouseOver 5)
                        |> renderHtml
                        |> expectSelectedText "a"
            , test "expand to left" <|
                \_ ->
                    createModel "gnu\nbar\nbaz"
                        |> whileSelectingAt 5
                        |> update (IT.MouseOver 4)
                        |> renderHtml
                        |> expectSelectedText "ba"
            ]
        ]


config : Config MyStyle () Msg
config =
    { lift = TextareaMsg
    , highlighter = emptyHighlighter
    , predictionConfig = Nothing
    }


update : IT.Msg -> Model MyStyle () -> Model MyStyle ()
update msg model =
    let
        ( m, _, _ ) =
            Textarea.update config msg model
    in
    m



--updateNoHl =
--    update emptyHighlighter


emptyHighlighter =
    \text ->
        []


renderer : List MyStyle -> List (Html.Attribute Msg)
renderer myStyles =
    myStyles
        |> List.foldl
            (\myStyle attrs ->
                case myStyle of
                    Style1 ->
                        attrs
                            ++ [ A.style "color" "grey"
                               ]

                    Style2 ->
                        attrs
                            ++ [ A.style "color" "#C086D0"
                               ]
            )
            []


createModel : String -> Model MyStyle ()
createModel str =
    Textarea.init
        { idPrefix = "test-ta"
        , initialText = str
        , debounceMs = 1000
        }
        |> Tuple.first


withSelection : Range -> Model s () -> Model s ()
withSelection range (IT.Model d) =
    IT.Model
        { d
            | selection = Just range
        }


whileSelectingAt : Int -> Model s () -> Model s ()
whileSelectingAt at (IT.Model d) =
    IT.Model
        { d
            | selectingAt = Just at
        }


renderHtml : Model MyStyle () -> Single Msg
renderHtml m =
    Textarea.view
        config
        m
        |> fromHtml


hasCaretAt : Int -> Selector
hasCaretAt pos =
    all
        [ attribute <| A.attribute "data-from" (fromInt pos)
        , containing
            [ class "blinking-cursor" ]
        , containing
            [ text " " ]
        ]


expectSelectedText : String -> Single Msg -> Expectation
expectSelectedText expected single =
    single
        |> findAll [ selectedDiv ]
        |> Expect.all
            (toList expected
                |> indexedMap
                    (\i c ->
                        index i >> has [ text <| fromChar c ]
                    )
            )


selectedDiv : Selector
selectedDiv =
    style "background-color" "lightblue"



-- TODO move to separate module
-- The problem is sharing utility functions, like createModel, etc.
-- Apparently test modules cannot consume other modules under the tests/ folder.


updateSuite : Test
updateSuite =
    describe "Update tests"
        [ describe "mouse selection"
            [ test "expand to end of line" <|
                \_ ->
                    createModel "foo\nbar\nbaz"
                        |> withSelection (range 3 3)
                        |> whileSelectingAt 3
                        |> update (IT.MouseUpLine 0)
                        |> getSelection
                        |> Expect.equal (Just <| range 3 3)
            , test "expand at end of line" <|
                \_ ->
                    createModel "foo\nbar\nbaz"
                        |> withSelection (range 3 3)
                        |> whileSelectingAt 3
                        |> update (IT.MouseOverLine 0)
                        |> getSelection
                        |> Expect.equal (Just <| range 3 3)
            , test "blur keeps selection" <|
                \_ ->
                    createModel "foo\nbar\nbaz"
                        |> withSelection (range 3 3)
                        |> update IT.Blurred
                        |> getSelection
                        |> Expect.equal (Just <| range 3 3)
            , test "expand to right" <|
                \_ ->
                    createModel "foo\nbar\nbaz"
                        |> whileSelectingAt 5
                        |> update (IT.MouseOver 5)
                        |> getSelection
                        |> Expect.equal (Just <| range 5 6)
            , test "expand to left" <|
                \_ ->
                    createModel "foo\nbar\nbaz"
                        |> whileSelectingAt 5
                        |> update (IT.MouseOver 4)
                        |> getSelection
                        |> Expect.equal (Just <| range 4 5)
            ]
        ]


getSelection : Model s () -> Maybe Range
getSelection (IT.Model d) =
    .selection d


getSelectingAt : Model s () -> Maybe Int
getSelectingAt (IT.Model d) =
    .selectingAt d
