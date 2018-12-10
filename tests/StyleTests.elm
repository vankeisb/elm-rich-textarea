module StyleTests exposing (MyStyle(..), expectStylesAt, myStr, suite, testStyle12, testStyle13)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Range exposing (Range, range)
import Internal.Styles as S exposing (StyledText, Styles, getStylesAt)
import Test exposing (..)


myStr =
    "0123456789"


testStyle12 =
    S.fromList
        [ ( range 1 2, MyStyle1 ) ]


testStyle13 =
    S.fromList
        [ ( range 1 3, MyStyle1 ) ]


type MyStyle
    = MyStyle1
    | MyStyle2


expectStylesAt index expected styles =
    \_ ->
        Expect.equalLists
            expected
            (getStylesAt index styles)


suite : Test
suite =
    describe "Style tests"
        [ describe "get styles at"
            [ test "0 - [1..2]" <|
                expectStylesAt 0 [] testStyle12
            , test "1 - [1..2]" <|
                expectStylesAt 1 [ MyStyle1 ] testStyle12
            , test "2 - [1..2]" <|
                expectStylesAt 2 [] testStyle12
            , test "3 - [1..2]" <|
                expectStylesAt 3 [] testStyle12
            , test "10 - [1..2]" <|
                expectStylesAt 10 [] testStyle12
            , test "0 - [1..3]" <|
                expectStylesAt 0 [] testStyle13
            , test "1 - [1..3]" <|
                expectStylesAt 1 [ MyStyle1 ] testStyle13
            , test "2 - [1..3]" <|
                expectStylesAt 2 [ MyStyle1 ] testStyle13
            , test "3 - [1..3]" <|
                expectStylesAt 3 [] testStyle13
            , test "12 - [11..13]" <|
                expectStylesAt 12
                    [ MyStyle1 ]
                    (S.fromList
                        [ ( range 11 13, MyStyle1 )
                        ]
                    )
            ]
        , describe "styled text"
            [ test "no styles" <|
                \_ ->
                    Expect.equalLists
                        [ StyledText myStr (range 0 10) [] ]
                        (S.applyToText myStr 0 S.empty)
            , test "single style, width = 1" <|
                \_ ->
                    Expect.equalLists
                        [ StyledText "0" (range 0 1) []
                        , StyledText "1" (range 1 2) [ MyStyle1 ]
                        , StyledText "23456789" (range 2 10) []
                        ]
                        (S.applyToText myStr 0 testStyle12)
            , test "single style, width = 2" <|
                \_ ->
                    Expect.equalLists
                        [ StyledText "0" (range 0 1) []
                        , StyledText "12" (range 1 3) [ MyStyle1 ]
                        , StyledText "3456789" (range 3 10) []
                        ]
                        (S.applyToText myStr 0 testStyle13)
            , test "second line, width = 2" <|
                \_ ->
                    Expect.equalLists
                        [ StyledText "0" (range 10 11) []
                        , StyledText "12" (range 11 13) [ MyStyle1 ]
                        , StyledText "3456789" (range 13 20) []
                        ]
                        (S.fromList
                            [ ( range 11 13, MyStyle1 ) ]
                            |> S.applyToText myStr 10
                        )
            ]
        ]
