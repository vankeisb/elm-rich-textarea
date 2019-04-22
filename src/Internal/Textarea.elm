module Internal.Textarea exposing
    ( Box
    , Model(..)
    , ModelData
    , Msg(..)
    , Uuid
    , encodeUuid
    , initialUuid
    , lineSize
    , nextUuid
    , uuidDecoder
    )

import Array
import Browser.Dom as Dom
import Debounce
import Internal.Predictions exposing (Predictions)
import Internal.Styles exposing (StyledText, Styles)
import Json.Decode as Decode
import Json.Encode as Encode
import Range exposing (Range)


type Msg
    = OnInput String Int Int
    | OnKeyDown Int Bool Int Int
    | OnKeyUp Int Bool Int Int
    | MouseDown Int
    | MouseUp Int
    | MouseOver Int
    | MouseMoveExpand Int
    | MouseDownLine Int
    | MouseOverLine Int
    | MouseUpLine Int
    | MouseClicks Int Float
    | BackgroundMouseDown
    | BackgroundMouseOver
    | BackgroundMouseUp
    | BackgroundMouseLeft
    | Focused (Result Dom.Error ())
    | Blurred
    | GetViewportPos (Result Dom.Error Dom.Element)
    | GetViewport (Result Dom.Error Dom.Viewport)
    | GetCharViewport (Result Dom.Error Dom.Element)
    | Scrolled Float Float
    | DebounceMsg Debounce.Msg
    | TriggerHighlight
    | GetPredictionCharViewport (Result Dom.Error Dom.Element)
    | PredictionClicked Int
    | NoOp


type Uuid
    = Uuid Int


initialUuid : Uuid
initialUuid =
    Uuid 0


nextUuid : Uuid -> Uuid
nextUuid (Uuid i) =
    Uuid (i + 1)


type alias ModelData s p =
    { idPrefix : String
    , text : String
    , selection : Maybe Range
    , styles : Styles s
    , styledTexts : List (List (StyledText s))
    , focused : Bool
    , viewportBox : Box
    , selectingAt : Maybe Int
    , highlightId : Uuid
    , debounce : Debounce.Debounce Uuid
    , debounceMs : Float
    , predictions : Predictions p
    , predictionId : Uuid
    }


type alias Box =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    , scrollTop : Float
    , scrollLeft : Float
    }


type Model s p
    = Model (ModelData s p)


lineSize : Int -> String -> Maybe Int
lineSize n text =
    String.split "\n" text
        |> List.map String.length
        |> List.foldl
            (\len ( total, res ) ->
                let
                    newTotal =
                        len + total + 1
                in
                ( newTotal, res ++ [ newTotal ] )
            )
            ( 0, [] )
        |> Tuple.second
        |> Array.fromList
        |> Array.get n


encodeUuid : Uuid -> Encode.Value
encodeUuid (Uuid id) =
    Encode.int id


uuidDecoder : Decode.Decoder Uuid
uuidDecoder =
    Decode.map Uuid Decode.int
