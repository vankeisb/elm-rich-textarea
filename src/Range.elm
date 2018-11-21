module Range exposing
    ( Range
    , contains
    , expand
    , getBounds
    , getFrom
    , isCaret
    , move
    , range
    )


type Range
    = Range Int Int


range : Int -> Int -> Range
range from to =
    Range from to


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


expand : Int -> Range -> Range
expand new (Range from to) =
    if new > from then
        Range from new

    else
        Range new to
