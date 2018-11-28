module UiTests exposing
    ( Msg(..)
    , MyStyle(..)
    , createModel
    , createModelNoHl
    , emptyHighlighter
    , renderHtml
    , renderer
    , suite
    , update
    , updateNoHl
    )

import Expect exposing (Expectation)
import Html
import Html.Attributes as A
import Internal.Textarea as IT
import Json.Encode as Encode
import Range exposing (Range, range)
import String exposing (fromInt)
import Test exposing (..)
import Test.Html.Event as E
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)
import Textarea exposing (..)


type Msg
    = TextareaMsg Textarea.Msg


type MyStyle
    = Style1
    | Style2


suite : Test
suite =
    describe "UI Tests"
        [ test "single line, no styles" <|
            \_ ->
                createModelNoHl "foo bar baz"
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
                createModelNoHl "foo\nbar\nbaz"
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
                    |> E.expect (TextareaMsg <| IT.LineClicked 1)
        , test "click eol should place caret at the end of the line and nowhere else" <|
            \_ ->
                createModelNoHl "foo\nbar\nbaz"
                    |> updateNoHl (IT.LineClicked 1)
                    |> Expect.all
                        [ \(IT.Model m) ->
                            Expect.equal
                                (Just <|
                                    Range.range 7 7
                                )
                                m.selection
                        ]
        , test "when clicking the bg then the caret should be at the end of text" <|
            \_ ->
                createModelNoHl "foo\nbar\nbaz"
                    |> updateNoHl IT.BackgroundClicked
                    |> renderHtml
                    |> has [ hasCaretAt 11 ]
        , test "when mouse down on the line then the caret should be at the end of this line" <|
            \_ ->
                createModelNoHl "foo\nbar\nbaz"
                    |> updateNoHl (IT.LineClicked 0)
                    |> renderHtml
                    |> has [ hasCaretAt 3 ]
        , test "when clicking (mouse up and down) on the line then the caret should be at the end of this line" <|
            \_ ->
                createModelNoHl "foo\nbar\nbaz"
                    |> withSelection (range 3 3)
                    |> whileSelectingAt 3
                    |> Debug.log "FW1"
                    |> updateNoHl (IT.MouseUpLine 0)
                    |> Debug.log "FW2"
                    |> renderHtml
                    |> has [ hasCaretAt 3 ]
        ]


update : Highlighter MyStyle -> IT.Msg -> Model MyStyle -> Model MyStyle
update hl msg model =
    Textarea.update hl msg model
        |> Tuple.first


updateNoHl =
    update emptyHighlighter


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


createModel : Highlighter MyStyle -> String -> Model MyStyle
createModel hl str =
    Textarea.init
        { idPrefix = "test-ta"
        , highlighter = hl
        , initialText = str
        }
        |> Tuple.first


createModelNoHl =
    createModel emptyHighlighter


withSelection : Range -> Model s -> Model s
withSelection range (IT.Model d) =
    IT.Model
        { d
            | selection = Just range
        }


whileSelectingAt : Int -> Model s -> Model s
whileSelectingAt at (IT.Model d) =
    IT.Model
        { d
            | selectingAt = Just at
        }


renderHtml : Model MyStyle -> Single Msg
renderHtml m =
    Textarea.view
        TextareaMsg
        (Textarea.attributedRenderer m TextareaMsg renderer)
        m
        |> fromHtml


hasCaretAt : Int -> Selector
hasCaretAt pos =
    all
        [ attribute <| A.attribute "data-from" (fromInt pos)
        , containing
            [ class "blinking-cursor" ]
        , containing
            [ text "\n" ]
        ]
