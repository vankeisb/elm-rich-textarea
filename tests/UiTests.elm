module UiTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Textarea
import Html
import Html.Attributes as A
import Test.Html.Query exposing (..)
import Test.Html.Selector exposing (..)
import Range exposing (Range)


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
        ]
