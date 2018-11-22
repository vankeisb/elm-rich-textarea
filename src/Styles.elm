module Styles exposing
    ( StyledText
    , Styles
    , addStyles
    , applyToText
    , empty
    , fromList
    , getStylesAt
    )

import Range exposing (Range)


type Styles s
    = Styles (List ( Range, s ))


empty : Styles s
empty =
    Styles []


addStyles : List ( Range, s ) -> Styles s -> Styles s
addStyles ss (Styles styles) =
    Styles <| ss ++ styles


fromList : List ( Range, s ) -> Styles s
fromList l =
    Styles l


getStylesAt : Int -> Styles s -> List s
getStylesAt i (Styles styles) =
    styles
        |> List.foldl
            (\( r, st ) acc ->
                if Range.contains i r then
                    -- style is in range
                    st :: acc

                else
                    -- style is not in range
                    acc
            )
            []


type alias StyledText s =
    { text : String
    , range : Range
    , styles : List s
    }



{-
   Apply styles to passed string and offset, and return a list
   of StyledTexts.
-}


applyToText : String -> Int -> Styles s -> List (StyledText s)
applyToText s offset styles =
    let
        handleChar : String -> String -> Int -> Int -> List s -> List (StyledText s) -> List (StyledText s)
        handleChar str buf index startIndex curStyles res =
            let
                -- get styles at current index
                newStyles =
                    getStylesAt index styles

                mkRange =
                    Range.range startIndex (startIndex + String.length buf)
            in
            case String.uncons str of
                Just ( c, rest ) ->
                    -- found char at index, check if we
                    -- need to close and open new styles here
                    if curStyles == newStyles then
                        -- same styles, accumulate into buffer and
                        -- continue scanning the string
                        handleChar
                            rest
                            (buf ++ String.fromChar c)
                            (index + 1)
                            startIndex
                            curStyles
                            res

                    else
                        -- styles change at this index : we
                        -- "close" the current styles, and
                        -- open new one
                        handleChar
                            rest
                            (String.fromChar c)
                            (index + 1)
                            index
                            newStyles
                            (res
                                ++ [ StyledText buf mkRange curStyles
                                   ]
                            )

                Nothing ->
                    -- string is finished, we can "close"
                    -- all the styles here
                    res
                        ++ [ StyledText buf mkRange curStyles ]
    in
    handleChar s "" offset offset [] []
