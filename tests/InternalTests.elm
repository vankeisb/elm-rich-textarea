module InternalTests exposing (suite)

import Expect exposing (Expectation)
import Internal.Textarea exposing (lineSize)
import Test exposing (..)


suite : Test
suite =
    describe "Internal.Textarea tests"
        [ describe "line size"
            [ test "no line" <|
                \_ ->
                    "foo"
                        |> lineSize 0
                        |> Expect.equal (Just 4)
            , test "missing line" <|
                \_ ->
                    "foo"
                        |> lineSize 1
                        |> Expect.equal Nothing
            , test "first of two lines" <|
                \_ ->
                    "foo\nbar baz"
                        |> lineSize 0
                        |> Expect.equal (Just 4)
            , test "second line" <|
                \_ ->
                    "foo\nbar baz"
                        |> lineSize 1
                        |> Expect.equal (Just 12)
            ]
        ]
