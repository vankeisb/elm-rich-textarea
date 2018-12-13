module Internal.Textarea exposing
    ( Box
    , Model(..)
    , ModelData
    , Msg(..)
    , ReturnStyles
    , StyleResolver
    , lineSize
    )

import Array
import Browser.Dom as Dom
import Debounce
import Html
import Range exposing (Range)
import Styles exposing (StyledText, Styles)


type Msg s
    = OnInput String Int Int
    | OnKeyDown Int Int Int
    | OnKeyUp Int Int Int
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
    | NoOp
    | RequestHighlight String
    | NewHighlight Int (List ( Range, s ))
    | DebounceMsg Debounce.Msg


type alias ModelData msg s =
    { idPrefix : String
    , text : String
    , selection : Maybe Range
    , styles : Styles s
    , styledTexts : List (List (StyledText s))
    , focused : Bool
    , viewportBox : Box
    , selectingAt : Maybe Int
    , highlightId : Int
    , debounce : Debounce.Debounce String
    , lift : Msg s -> msg
    , resolveStyles : StyleResolver msg s
    }


type alias Box =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    , scrollTop : Float
    , scrollLeft : Float
    }


type alias StyleResolver msg s =
    List s -> List (Html.Attribute msg)


type alias ReturnStyles msg s =
    List ( Range, s ) -> Cmd msg


type Model msg s
    = Model (ModelData msg s)


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
