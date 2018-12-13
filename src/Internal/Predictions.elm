module Internal.Predictions exposing (Predictions(..), Prediction)


import Browser.Dom as Dom



type alias Prediction = String


type Predictions
    = Closed
    | Loading Dom.Element
    | Open Dom.Element (List Prediction)