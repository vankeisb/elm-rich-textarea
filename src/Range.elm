module Range exposing
    ( Range
    , contains
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


move : Int -> Range -> Range
move i (Range from to) =
    Range (from + i) (to + i)


insertAt : Int -> Range -> Range
insertAt pos (Range from to) =
    if pos < from then
        Range (from + 1) (to + 1)

    else if pos <= to then
        Range from (to + 1)

    else
        Range from to
