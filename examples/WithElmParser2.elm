module WithElmParser2 exposing (Model, Msg(..), MyStyle(..), config, highlight, init, renderer, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Range exposing (Range)
import Textarea2 as Textarea



{-
   Using the textarea in pure Elm.
-}


type Msg
    = TextareaMsg (Textarea.Msg MyStyle () Msg)
    | TextClicked



{-
   Custom user styles
-}


type MyStyle
    = Keyword
    | Identifier



{-
   Your Model should keep the textarea's Model, that's parent/child...
-}


type alias Model =
    { textareaModel : Textarea.Model MyStyle ()
    }


init : String -> ( Model, Cmd Msg )
init idPrefix =
    let
        ( m, c ) =
            Textarea.init <|
                Textarea.defaultInitData
                    idPrefix
                    "let\n  foo = 1\nin\n  foo + bar"
    in
    ( { textareaModel = m
      }
    , Cmd.map TextareaMsg c
    )


config : Textarea.ViewConfig MyStyle () Msg
config =
    { lift = TextareaMsg
    , highlighter = renderer
    , predictionConfig = Nothing
    }


updateConfig : Textarea.UpdateConfig MyStyle () Msg
updateConfig =
    { lift = TextareaMsg
    , predictionConfig = Nothing
    , requestHighlight =
        \applyStyles text ->
            highlight text
                |> applyStyles
    }


view : Model -> Html Msg
view model =
    div
        []
        [ h2
            []
            [ text "Elm highlight, no predictions" ]
        , div
            [ style "width" "400px"
            , style "height" "200px"
            , style "position" "relative"
            , style "border" "1px solid lightgray"
            ]
            [ Textarea.view config model.textareaModel
            ]
        ]


renderer : List MyStyle -> List (Html.Attribute Msg)
renderer myStyles =
    myStyles
        |> List.foldl
            (\myStyle attrs ->
                case myStyle of
                    Keyword ->
                        attrs
                            ++ [ style "color" "grey"
                               , style "font-weight" "bold"
                               , onClick TextClicked
                               ]

                    Identifier ->
                        attrs
                            ++ [ style "color" "#C086D0"
                               ]
            )
            []


highlight : String -> List ( Range, MyStyle )
highlight text =
    let
        stylify style word =
            String.indexes word text
                |> List.map
                    (\i ->
                        ( Range.range i (i + String.length word)
                        , style
                        )
                    )

        stylifyMany style words =
            words
                |> List.map (stylify style)
                |> List.concat

        keywords =
            stylifyMany Keyword
                [ "if"
                , "then"
                , "else"
                , "let"
                , "in"
                , "module"
                ]

        identifiers =
            stylifyMany Identifier
                [ "foo"
                , "bar"
                ]
    in
    --    []
    keywords ++ identifiers


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextareaMsg sub ->
            let
                ( tm, c ) =
                    Textarea.update updateConfig sub model.textareaModel
            in
            ( { model
                | textareaModel =
                    tm
              }
            , c
            )

        TextClicked ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TextareaMsg <| Textarea.subscriptions model.textareaModel
        , Sub.none
        ]
