module Internal.Textarea exposing
    ( Box
    , HighlightId
    , Model(..)
    , ModelData
    , Msg(..)
    , encodeHighlightId
    , highlightIdDecoder
    , initialHighlightId
    , lineSize
    )

import Array
import Browser.Dom as Dom
import Debounce
import Internal.Styles exposing (StyledText, Styles)
import Internal.Predictions exposing (Predictions)
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
    | MouseDownLine Int
    | MouseOverLine Int
    | MouseUpLine Int
    | MouseClicks Int Float
    | BackgroundMouseDown
    | BackgroundMouseOver
    | BackgroundMouseUp
    | BackgroundMouseLeft
    | BackgroundMouseEnter Int
    | Focused (Result Dom.Error ())
    | Blurred
    | GetViewportPos (Result Dom.Error Dom.Element)
    | GetViewport (Result Dom.Error Dom.Viewport)
    | GetCharViewport (Result Dom.Error Dom.Element)
    | Scrolled Float Float
    | DebounceMsg Debounce.Msg
    | TriggerHighlight
    | GetPredictionCharViewport (Result Dom.Error Dom.Element)
    | NoOp


type HighlightId
    = HighlightId Int


initialHighlightId : HighlightId
initialHighlightId =
    HighlightId 0



type alias ModelData s p =
    { idPrefix : String
    , text : String
    , selection : Maybe Range
    , styles : Styles s
    , styledTexts : List (List (StyledText s))
    , focused : Bool
    , viewportBox : Box
    , selectingAt : Maybe Int
    , highlightId : HighlightId
    , debounce : Debounce.Debounce HighlightId
    , debounceMs : Float
    , predictions: Predictions p

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


encodeHighlightId : HighlightId -> Encode.Value
encodeHighlightId (HighlightId id) =
    Encode.int id


highlightIdDecoder : Decode.Decoder HighlightId
highlightIdDecoder =
    Decode.map HighlightId Decode.int
