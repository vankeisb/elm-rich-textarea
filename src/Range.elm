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


type Range
    = Range Int Int


range : Int -> Int -> Range
range from to =
    if from < to then
        Range from to

    else
        Range to from


expand : Int -> Int -> Range
expand to at =
    if to <= at then
        Range to at

    else
        Range at to


contains : Int -> Range -> Bool
contains index (Range from to) =
    (index >= from) && (index < to)


getBounds : Range -> ( Int, Int )
getBounds (Range from to) =
    ( from, to )


getFrom : Range -> Int
getFrom (Range from _) =
    from


isCaret : Int -> Range -> Bool
isCaret i (Range from to) =
    (from == to) && (from == i)


empty : Range -> Bool
empty (Range from to) =
    from == to


move : Int -> Range -> Range
move i (Range from to) =
    Range (from + i) (to + i)


insertAt : Int -> Int -> Range -> Range
insertAt pos count (Range from to) =
    if pos < from then
        range (from + count) (to + count)

    else if pos <= to + 1 then
        range from (to + count)

    else
        range from to
