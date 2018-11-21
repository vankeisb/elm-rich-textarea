module RangeTests exposing (suite)

import Expect exposing (Expectation)
import Range exposing (Range, expand, range)
import Test exposing (..)


suite : Test
suite =
    describe "Range tests"
        [ describe "expand"
            [ test "to right" <|
                \_ ->
                    range 1 2
                        |> expand 13
                        |> Expect.equal (range 1 13)
            , test "to left" <|
                \_ ->
                    range 13 20
                        |> expand 3
                        |> Expect.equal (range 3 20)
            ]
        ]
