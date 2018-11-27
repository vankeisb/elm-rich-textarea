module UiTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Textarea
import Internal.Textarea as IT
import Html
import Html.Attributes as A
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)
import Range exposing (Range)
import Test.Html.Event as E
import Json.Encode as Encode

type Msg
    = TextareaMsg Textarea.Msg


type MyStyle
    = Style1
    | Style2


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

suite: Test
suite =
    describe "UI Tests"
        [ test "single line, no styles" <|
            \_ ->
                Textarea.init
                    { idPrefix = "my-ta"
                    , highlighter =
                        \text ->
                            []
                    , initialText = "foo bar baz"
                    }
                    |> Tuple.first
                    |>
                        \m ->
                            Textarea.view
                                TextareaMsg
                                (Textarea.attributedRenderer m TextareaMsg renderer)
                                m
                    |> fromHtml
                    |> Expect.all
                        ( "foo bar baz"
                            |> String.toList
                            |> List.indexedMap
                                (\i c ->
                                    \html ->
                                        html
                                            |> find
                                                [ attribute
                                                    <| A.attribute "data-from" (String.fromInt i)
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
                            [ ("offsetX", Encode.float 0)
                            ,
                                ( "target"
                                , Encode.object
                                    [ ("clientWidth", Encode.int 0) ]
                                )
                            ]
                in
                Textarea.init
                    { idPrefix = "my-ta"
                    , highlighter = emptyHighlighter
                    , initialText = "foo\nbar\nbaz"
                    }
                    |> Tuple.first
                    |>
                        \m ->
                            Textarea.view
                                TextareaMsg
                                (Textarea.attributedRenderer m TextareaMsg renderer)
                                m
                    |> fromHtml
                    |> find
                        [ style "display" "flex"
                        , containing
                            [ attribute
                                <| A.attribute "data-from" "6"
                            , text "r"
                            ]
                        ]
                    |> E.simulate
                        (E.custom "mousedown" simulatedEvent)
                    |> E.expect (TextareaMsg <| IT.LineClicked 1)
        , test "click eol should place caret at the end of the line and nowhere else" <|
            \_ ->
                Textarea.init
                    { idPrefix = "my-ta"
                    , highlighter = emptyHighlighter
                    , initialText = "foo\nbar\nbaz"
                    }
                    |> Tuple.first
                    |> Textarea.update
                        emptyHighlighter
                        (IT.LineClicked 1)
                    |> Tuple.first
                    |> Expect.all
                        [ \(IT.Model m) ->
                            Expect.equal
                                ( Just <|
                                    Range.range 7 7
                                )
                                m.selection
                        ]
        , test "when selection is at eol then the caret should blink" <|
            \_ ->
                Textarea.init
                    { idPrefix = "my-ta"
                    , highlighter = emptyHighlighter
                    , initialText = "foo\nbar\nbaz"
                    }
                    |> Tuple.first
                    |> Textarea.update
                        emptyHighlighter
                        (IT.BackgroundClicked)
                    |> Tuple.first
                    |>
                        \m ->
                            Textarea.view
                                TextareaMsg
                                (Textarea.attributedRenderer m TextareaMsg renderer)
                                m
                    |> fromHtml
                    |> has
                        [ attribute <| A.attribute "data-from" "11"
                        , containing
                            [ class "blinking-cursor"
                            , containing
                                [ text "\n"
                                ]
                            ]
--                            , all
--                                [ class "blinking-cursor"
--                                , text ""
--                                ]
                        ]
        ]


emptyHighlighter =
    \text ->
        []