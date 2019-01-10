module Internal.Predictions exposing
    ( Predictions(..)
    , PredictionsData
    , applyFilter
    , fromList
    , getInitialCaretPos
    , getSelected
    , isSelected
    , moveDown
    , moveUp
    , setSelected
    , toList
    )

import Browser.Dom as Dom


type PredictionsData p
    = PredictionsData
        { start : List p
        , selected : Maybe p
        , end : List p
        , initialCaretPos : Int
        , allItems : List p
        }


toList : PredictionsData p -> List p
toList (PredictionsData d) =
    d.start
        ++ (d.selected
                |> Maybe.map (\p -> [ p ])
                |> Maybe.withDefault []
           )
        ++ d.end


fromList : Int -> List p -> PredictionsData p
fromList caretPos ps =
    PredictionsData
        { start = []
        , selected = List.head ps
        , end =
            List.tail ps
                |> Maybe.withDefault []
        , initialCaretPos = caretPos
        , allItems = ps
        }
        |> selectFirst


selectFirst : PredictionsData p -> PredictionsData p
selectFirst (PredictionsData d) =
    case d.start of
        p :: ps ->
            PredictionsData
                { d
                    | start = []
                    , selected = Just p
                    , end =
                        ps
                            ++ (d.selected
                                    |> Maybe.map (\pred -> [ pred ])
                                    |> Maybe.withDefault []
                               )
                            ++ d.end
                }

        [] ->
            PredictionsData d


isSelected : p -> PredictionsData p -> Bool
isSelected p (PredictionsData d) =
    d.selected == Just p


getInitialCaretPos : PredictionsData p -> Int
getInitialCaretPos (PredictionsData pd) =
    pd.initialCaretPos


getSelected : PredictionsData p -> Maybe p
getSelected (PredictionsData pd) =
    pd.selected


type Predictions p
    = Closed
    | Loading Dom.Element
    | Open Dom.Element (PredictionsData p)


moveUp : PredictionsData p -> PredictionsData p
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


moveDown : PredictionsData p -> PredictionsData p
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


applyFilter : (p -> String) -> String -> PredictionsData p -> PredictionsData p
applyFilter predictionText filterString (PredictionsData pd) =
    let
        preds =
            pd.allItems

        filtered =
            pd.allItems
                |> List.filter
                    (\p ->
                        String.startsWith filterString (predictionText p)
                    )

        previouslySelectedText =
            pd.selected
                |> Maybe.map predictionText

        ( start, sel, end ) =
            filtered
                |> List.foldl
                    (\pred ( st, sl, ed ) ->
                        case sl of
                            Just _ ->
                                -- already a selected item, append to "end"
                                ( st, sl, ed ++ [ pred ] )

                            Nothing ->
                                -- no selected item yet, check if this is the one !
                                if previouslySelectedText == Just (predictionText pred) then
                                    -- yep, this is the one !
                                    ( st, Just pred, [] )

                                else
                                    -- not the one, append to "start"
                                    ( st ++ [ pred ], Nothing, [] )
                    )
                    ( [], Nothing, [] )

        newPd =
            PredictionsData
                { pd
                    | start = start
                    , selected = sel
                    , end = end
                }
    in
    if sel == Nothing then
        selectFirst newPd

    else
        newPd


setSelected : p -> Predictions p -> Predictions p
setSelected p preds =
    case preds of
        Open e pdata ->
            let
                (PredictionsData pd) =
                    pdata

                ( start, sel, end ) =
                    toList pdata
                        |> List.foldl
                            (\pred ( st, sl, ed ) ->
                                case sl of
                                    Just _ ->
                                        -- already a selected item, append to "end"
                                        ( st, sl, ed ++ [ pred ] )

                                    Nothing ->
                                        -- no selected item yet, check if this is the one !
                                        if pred == p then
                                            -- yep, this is the one !
                                            ( st, Just pred, [] )

                                        else
                                            -- not the one, append to "start"
                                            ( st ++ [ pred ], Nothing, [] )
                            )
                            ( [], Nothing, [] )
            in
            Open e <|
                PredictionsData
                    { pd
                        | start = start
                        , selected = sel
                        , end = end
                    }

        _ ->
            preds
