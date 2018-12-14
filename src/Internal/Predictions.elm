module Internal.Predictions exposing
    ( Predictions(..)
    , PredictionsData
    , toList
    , fromList
    , isSelected
    , moveUp
    , moveDown
    )


import Browser.Dom as Dom


type PredictionsData p =
    PredictionsData
        { start: List p
        , selected: Maybe p
        , end: List p
        }


toList: PredictionsData p -> List p
toList (PredictionsData d) =
    d.start ++
        (d.selected
            |> Maybe.map (\p -> [ p ])
            |> Maybe.withDefault []
        ) ++ d.end


fromList: List p -> PredictionsData p
fromList ps =
    PredictionsData
        { start = []
        , selected = List.head ps
        , end =
            List.tail ps
                |> Maybe.withDefault []
        }


isSelected: p -> PredictionsData p -> Bool
isSelected p (PredictionsData d) =
    d.selected == Just p



type Predictions p
    = Closed
    | Loading Dom.Element
    | Open Dom.Element (PredictionsData p)


moveUp: PredictionsData p -> PredictionsData p
moveUp (PredictionsData d) =
    PredictionsData <|
        if List.isEmpty d.start then
            d
        else
            case d.selected of
                Just selected ->
                    { d
                        | start =
                            d.start
                                |> List.reverse
                                |> List.tail
                                |> Maybe.withDefault []
                                |> List.reverse
                        , selected =
                            d.start
                                |> List.reverse
                                |> List.head
                        , end =
                            selected :: d.end
                    }
                Nothing ->
                    d


moveDown: PredictionsData p -> PredictionsData p
moveDown (PredictionsData d) =
    PredictionsData <|
        if List.isEmpty d.end then
            d
        else
            case d.selected of
                Just selected ->
                    { d
                        | start =
                            d.start ++ [ selected ]
                        , selected =
                            List.head d.end
                        , end =
                            d.end
                                |> List.tail
                                |> Maybe.withDefault []
                    }
                Nothing ->
                    d