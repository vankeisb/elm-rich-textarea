module Internal.Textarea exposing
    ( Msg(..)
    , ModelData
    , Box
    , Model(..)
    )


import Browser.Dom as Dom
import Styles exposing (Styles, StyledText)
import Range exposing (Range)

type Msg
    = OnInput String Int Int
    | OnKeyDown Int Int Int
    | OnKeyUp Int Int Int
    | MouseDown Int
    | MouseUp Int
    | MouseOver Int
    | MouseOverLine Int
    | MouseUpLine Int
    | BackgroundClicked
    | BackgroundMouseOver
    | BackgroundMouseUp
    | LineClicked Int
    | Focused (Result Dom.Error ())
    | Blurred
    | GetViewportPos (Result Dom.Error Dom.Element)
    | GetViewport (Result Dom.Error Dom.Viewport)
    | GetCharViewport (Result Dom.Error Dom.Element)
    | Scrolled Float Float
    | NoOp


type alias ModelData s =
    { idPrefix : String
    , text : String
    , selection : Maybe Range
    , styles : Styles s
    , styledTexts : List (List (StyledText s))
    , focused : Bool
    , viewportBox : Box
    , selectingAt : Maybe Int
    }


type alias Box =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    , scrollTop : Float
    , scrollLeft : Float
    }



type Model s
    = Model (ModelData s)