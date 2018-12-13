module Internal.Predictions exposing (Predictions(..))


import Browser.Dom as Dom


type Predictions
    = Closed
    | Loading Dom.Element