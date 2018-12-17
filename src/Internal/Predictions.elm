module Internal.Predictions exposing
    ( Predictions(..)
    , PredictionsData
    , toList
    , fromList
    , isSelected
    , moveUp
    , moveDown
    , getInitialCaretPos
    , applyFilter
    )


import Browser.Dom as Dom


type PredictionsData p =
    PredictionsData
        { start: List p
        , selected: Maybe p
        , end: List p
        , initialCaretPos: Int
        , allItems: List p
        }


toList: PredictionsData p -> List p
toList (PredictionsData d) =
    d.start ++
        (d.selected
            |> Maybe.map (\p -> [ p ])
            |> Maybe.withDefault []
        ) ++ d.end


fromList: Int -> List p -> PredictionsData p
fromList caretPos ps =
    PredictionsData
        { start = []
        , selected = Nothing
        , end = []
        , initialCaretPos = caretPos
        , allItems = ps
        }
        |> applyFilter ""


isSelected: p -> PredictionsData p -> Bool
isSelected p (PredictionsData d) =
    d.selected == Just p


getInitialCaretPos: PredictionsData p -> Int
getInitialCaretPos (PredictionsData pd) =
    pd.initialCaretPos



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


applyFilter: String -> PredictionsData p -> PredictionsData p
applyFilter s (PredictionsData pd) =
    let
        preds =
            pd.allItems

        x =
            Debug.log "applyFilter" s
    in
    PredictionsData
        { pd
            | start = []
            , selected = List.head preds
            , end =
                List.tail preds
                    |> Maybe.withDefault []
        }

