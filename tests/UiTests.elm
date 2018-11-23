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
                let
                    (m,c) =
                        Textarea.init
                            { idPrefix = "my-ta"
                            , highlighter =
                                \text ->
                                    []
                            , initialText = "foo bar baz"
                            }

                in
                Textarea.view
                    TextareaMsg
                    (Textarea.attributedRenderer m TextareaMsg renderer)
                    m
                    |> fromHtml
                    |> find [ attribute <| A.attribute "data-from" "0" ]
                    |> children [ text "f" ]
                    |> count (Expect.equal 1)
        ]


--
--makeExpected : String -> List (Range, MyStyle) ->
--makeExpected s =
--    []