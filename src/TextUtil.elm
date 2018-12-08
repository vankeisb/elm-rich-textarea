module TextUtil exposing (lineRangeAt, wordRangeAt)

import Range exposing (Range, range)


wordRangeAt : Int -> String -> Maybe Range
wordRangeAt =
    delimitedRangeAt isWhitespace


lineRangeAt : Int -> String -> Maybe Range
lineRangeAt =
    delimitedRangeAt isNewline


delimitedRangeAt : (Char -> Bool) -> Int -> String -> Maybe Range
delimitedRangeAt isDelim pos text =
    let
        indexed =
            text
                |> String.toList
                |> List.indexedMap (\i c -> ( i, c ))

        left =
            indexed
                |> List.filter (\( i, c ) -> i < pos && isDelim c)
                |> List.map Tuple.first
                |> List.reverse
                |> List.head
                |> Maybe.withDefault -1

        right =
            indexed
                |> List.filter (\( i, c ) -> i >= pos && isDelim c)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault (String.length text)
    in
    if String.isEmpty text || right == pos then
        Nothing

    else if left < right then
        Just <| range (left + 1) right

    else
        Nothing


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || isNewline c


isNewline : Char -> Bool
isNewline c =
    c == '\n'


maxIndexWith : (b -> Bool) -> List ( Int, b ) -> Int
maxIndexWith pred list =
    list
        |> List.foldl
            (\( i, c ) max ->
                if pred c && i > max then
                    i

                else
                    max
            )
            -1
