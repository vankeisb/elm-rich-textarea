module Internal.Predictions exposing (Predictions(..), Prediction)


import Browser.Dom as Dom



type alias Prediction = String


type alias PredictionsData =
    { predictions: List Prediction
    }



type Predictions
    = Closed
    | Loading Dom.Element
    | Open Dom.Element PredictionsData