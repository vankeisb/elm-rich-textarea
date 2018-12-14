module Internal.Predictions exposing (Predictions(..), PredictionsData)


import Browser.Dom as Dom


type alias PredictionsData p =
    { predictions: List p
    }



type Predictions p
    = Closed
    | Loading Dom.Element
    | Open Dom.Element (PredictionsData p)