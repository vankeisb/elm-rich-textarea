module TextUtilTests exposing (suite)

import Expect exposing (Expectation)
import Range exposing (range)
import Test exposing (..)
import TextUtil exposing (lineRangeAt, wordRangeAt)


suite : Test
suite =
    describe "TextUtil tests"
        [ describe "words"
            [ test "empty" <|
                \_ ->
                    ""
                        |> wordRangeAt 0
                        |> Expect.equal Nothing
            , test "only word" <|
                \_ ->
                    "word"
                        |> wordRangeAt 0
                        |> Expect.equal (Just <| range 0 4)
            , test "inside word" <|
                \_ ->
                    "foo bar gnu"
                        |> wordRangeAt 5
                        |> Expect.equal (Just <| range 4 7)
            , test "beginning of word" <|
                \_ ->
                    "foo bar gnu"
                        |> wordRangeAt 4
                        |> Expect.equal (Just <| range 4 7)
            , test "end of word" <|
                \_ ->
                    "foo bar gnu"
                        |> wordRangeAt 6
                        |> Expect.equal (Just <| range 4 7)
            , test "first word" <|
                \_ ->
                    "foo bar gnu"
                        |> wordRangeAt 2
                        |> Expect.equal (Just <| range 0 3)
            , test "last word" <|
                \_ ->
                    "foo bar gnu"
                        |> wordRangeAt 9
                        |> Expect.equal (Just <| range 8 11)
            , test "one char word" <|
                \_ ->
                    "foo b gnu"
                        |> wordRangeAt 4
                        |> Expect.equal (Just <| range 4 5)
            , test "between words" <|
                \_ ->
                    "foo b gnu"
                        |> wordRangeAt 3
                        |> Expect.equal Nothing
            , test "on operator" <|
                \_ ->
                    "foo + gnu"
                        |> wordRangeAt 4
                        |> Expect.equal (Just <| range 4 5)
            ]
        , describe "lines"
            [ test "empty" <|
                \_ ->
                    ""
                        |> lineRangeAt 0
                        |> Expect.equal Nothing
            , test "only line" <|
                \_ ->
                    "line"
                        |> lineRangeAt 0
                        |> Expect.equal (Just <| range 0 4)
            , test "inside line" <|
                \_ ->
                    "foo\nbar\ngnu"
                        |> lineRangeAt 5
                        |> Expect.equal (Just <| range 4 7)
            ]
        ]
