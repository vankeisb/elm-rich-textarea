module Main exposing (Model, Msg(..), MyStyle(..), highlighter, init, main, renderer, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Range exposing (Range)
import Styles
import Task
import Textarea


type Msg
    = TextareaMsg (Textarea.Msg MyStyle)
    | TextClicked


type MyStyle
    = Keyword
    | Identifier


type alias Model =
    { textareaModel : Textarea.Model MyStyle
    }


init : String -> ( Model, Cmd Msg )
init idPrefix =
    let
        ( m, c ) =
            Textarea.init
                { idPrefix = "my-ta"
                , highlighter = highlighter
                , initialText = "let\n  foo = 1\nin\n  foo + bar"
                }
    in
    ( { textareaModel = m
      }
    , Cmd.map TextareaMsg c
    )


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "This is a textarea... with style ! " ]
        , div
            [ style "width" "400px"
            , style "height" "200px"
            , style "position" "relative"
            , style "border" "1px solid lightgray"
            ]
            [ Textarea.view
                TextareaMsg
                (Textarea.attributedRenderer model.textareaModel TextareaMsg renderer)
                model.textareaModel
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


highlighter : String -> List ( Range, MyStyle )
highlighter text =
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
                updateData =
                    { lift = TextareaMsg
                    , onHighlight = onHighlight
                    }

                ( tm, c ) =
                    Textarea.update updateData sub model.textareaModel
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
    Sub.map TextareaMsg <|
        Textarea.subscriptions model.textareaModel


main =
    Browser.element
        { init =
            \() ->
                init "my-textarea"
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


onHighlight : (List ( Range, MyStyle ) -> Cmd Msg) -> String -> Cmd Msg
onHighlight return text =
    -- TODO try another version using ports
    return (highlighter text)
