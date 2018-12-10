module Range exposing
    ( Range
    , contains
    , empty
    , expand
    , getBounds
    , getFrom
    , insertAt
    , isCaret
    , move
    , range
    )


{-| Ranges "from" -> "to".

-}


{-| Range opaque type.
-}
type Range
    = Range Int Int


{-| Create a range
-}
range : Int -> Int -> Range
range from to =
    if from < to then
        Range from to

    else
        Range to from


{-| Expand passed range -}
expand : Int -> Int -> Range
expand to at =
    if to <= at then
        Range to at

    else
        Range at to


{-| Return true if range contains passed index -}
contains : Int -> Range -> Bool
contains index (Range from to) =
    (index >= from) && (index < to)



{-| Return (from,to) for passed range -}
getBounds : Range -> ( Int, Int )
getBounds (Range from to) =
    ( from, to )



{-| Return "from" bound for the range -}
getFrom : Range -> Int
getFrom (Range from _) =
    from


{-| Return true if the range is a caret at passed index -}
isCaret : Int -> Range -> Bool
isCaret i (Range from to) =
    (from == to) && (from == i)


{-| Determines if range is empty (width == 0) -}
empty : Range -> Bool
empty (Range from to) =
    from == to


{-| Move a Range with passed int -}
move : Int -> Range -> Range
move i (Range from to) =
    Range (from + i) (to + i)


{-| Insert count at position -}
insertAt : Int -> Int -> Range -> Range
insertAt pos count (Range from to) =
    if pos < from then
        range (from + count) (to + count)

    else if count > 0 && pos <= to + 1 then
        range from (to + count)

    else if count < 0 && pos <= to - 1 then
        range from (to + count)

    else
        range from to
