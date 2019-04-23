module TextUtil exposing
    ( countLeadingNonWhitespace
    , lineRangeAt
    , maybeOrElse
    , wordRangeAt
    )

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
                |> List.foldr
                    (\ic acc ->
                        acc
                            |> maybeOrElse (candidatePosition (atLeft isDelim pos) ic)
                    )
                    Nothing
                |> Maybe.withDefault -1

        right =
            indexed
                |> List.foldl
                    (\ic acc ->
                        acc
                            |> maybeOrElse (candidatePosition (atRight isDelim pos) ic)
                    )
                    Nothing
                |> Maybe.withDefault (String.length text)
    in
    if String.isEmpty text || right == pos then
        Nothing

    else if left < right then
        Just <| range (left + 1) right

    else
        Nothing


maybeOrElse : Maybe a -> Maybe a -> Maybe a
maybeOrElse recover maybe =
    maybe
        |> Maybe.map Just
        |> Maybe.withDefault recover


candidatePosition : (( Int, Char ) -> Bool) -> ( Int, Char ) -> Maybe Int
candidatePosition pred ( i, c ) =
    if pred ( i, c ) then
        Just i

    else
        Nothing


atRight : (Char -> Bool) -> Int -> (( Int, Char ) -> Bool)
atRight isDelim pos ( i, c ) =
    i >= pos && isDelim c


atLeft : (Char -> Bool) -> Int -> (( Int, Char ) -> Bool)
atLeft isDelim pos ( i, c ) =
    i < pos && isDelim c


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


countLeadingNonWhitespace : String -> Int
countLeadingNonWhitespace text =
    text
        |> String.toList
        |> List.foldl
            (\c ( count, done ) ->
                if done || isWhitespace c then
                    ( count, True )

                else
                    ( count + 1, False )
            )
            ( 0, False )
        |> Tuple.first
