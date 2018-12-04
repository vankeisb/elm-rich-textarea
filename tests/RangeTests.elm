module RangeTests exposing (suite)

import Expect exposing (Expectation)
import Range exposing (Range, empty, expand, insertAt, range)
import Test exposing (..)


suite : Test
suite =
    describe "Range tests"
        [ describe "init (and normalize)"
            [ test "normal" <|
                \_ ->
                    range 1 2
                        |> Expect.equal (range 2 1)
            ]
        , describe "expand"
            [ test "expand same" <|
                \_ ->
                    expand 1 1
                        |> Expect.equal (range 1 1)
            , test "expand to left" <|
                \_ ->
                    expand 2 1
                        |> Expect.equal (range 1 2)
            , test "expand to right" <|
                \_ ->
                    expand 12 13
                        |> Expect.equal (range 12 13)
            ]
        , describe "insert"
            [ test "before" <|
                \_ ->
                    range 13 15
                        |> insertAt 5 1
                        |> Expect.equal (range 14 16)
            , test "after" <|
                \_ ->
                    range 13 15
                        |> insertAt 20 1
                        |> Expect.equal (range 13 15)
            , test "inside" <|
                \_ ->
                    range 13 15
                        |> insertAt 14 1
                        |> Expect.equal (range 13 16)
            , test "left boundary" <|
                \_ ->
                    range 13 15
                        |> insertAt 13 1
                        |> Expect.equal (range 13 16)
            , test "right boundary" <|
                \_ ->
                    range 13 15
                        |> insertAt 16 1
                        |> Expect.equal (range 13 16)
            ]
        , describe "delete"
            [ test "before" <|
                \_ ->
                    range 13 15
                        |> insertAt 5 -1
                        |> Expect.equal (range 12 14)
            , test "after" <|
                \_ ->
                    range 13 15
                        |> insertAt 20 -1
                        |> Expect.equal (range 13 15)
            , test "inside" <|
                \_ ->
                    range 13 15
                        |> insertAt 14 -1
                        |> Expect.equal (range 13 14)
            , test "left boundary" <|
                \_ ->
                    range 13 15
                        |> insertAt 13 -1
                        |> Expect.equal (range 13 14)
            , test "right boundary" <|
                \_ ->
                    range 13 15
                        |> insertAt 16 -1
                        |> Expect.equal (range 13 14)
            , test "vanish" <|
                \_ ->
                    range 13 14
                        |> insertAt 14 -1
                        |> empty
                        |> Expect.equal True
            , test "vanish 2" <|
                \_ ->
                    range 13 14
                        |> insertAt 13 -1
                        |> empty
                        |> Expect.equal True
            ]
        ]
