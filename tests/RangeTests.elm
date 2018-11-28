module RangeTests exposing (suite)

import Expect exposing (Expectation)
import Range exposing (Range, expand, range)
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
        ]
