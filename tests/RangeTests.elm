module RangeTests exposing (suite)

import Expect exposing (Expectation)
import Range exposing (Range, range)
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
        ]
