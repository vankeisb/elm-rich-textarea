module Internal.Textarea exposing
    ( Box
    , Model(..)
    , ModelData
    , Msg(..)
    , lineSize
    )

import Array
import Browser.Dom as Dom
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
    | BackgroundClicked
    | BackgroundMouseOver
    | BackgroundMouseUp
    | Focused (Result Dom.Error ())
    | Blurred
    | GetViewportPos (Result Dom.Error Dom.Element)
    | GetViewport (Result Dom.Error Dom.Viewport)
    | GetCharViewport (Result Dom.Error Dom.Element)
    | Scrolled Float Float
    | NoOp
    | NewStyles (List ( Range, s ))


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
