module Main exposing (..)


import Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Range exposing (Range)
import Styles


type Msg
    = TextareaMsg Textarea.Msg
    | TextClicked



type MyStyle
    = Keyword
    | Identifier



type alias Model =
    { textareaModel: Textarea.Model MyStyle
    }



init : String -> (Model, Cmd Msg)
init idPrefix =
    let
        (m, c) =
            Textarea.init
                { idPrefix = "my-ta"
                , highlighter = highlighter
                , initialText = "let\n  foo = 1\nin\n  foo + bar"
                }
    in
    (
        { textareaModel = m
        }
    , Cmd.map TextareaMsg c
    )



view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "This is a textarea... with style ! "]
        , div
            [ style "width" "500px"
            , style "height" "100px"
            , style "position" "relative"
            , style "border" "1px solid lightgray"
            ]
            [ Textarea.view
                TextareaMsg
                (Textarea.attributedRenderer TextareaMsg renderer)
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
                        attrs ++
                            [ style "color" "grey"
                            , style "font-weight" "bold"
                            , onClick TextClicked
                            ]
                    Identifier ->
                        attrs ++
                            [ style "color" "#C086D0"
                            ]
            )
            []



highlighter text =
    let
        stylify style word =
            String.indexes word text
                |> List.map
                    (\i ->
                        ( Range.range i (i + (String.length word))
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
    keywords ++ identifiers


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextareaMsg sub ->
            let
                (tm, tc) =
                    Textarea.update highlighter sub model.textareaModel
            in
            (
                { model
                    | textareaModel =
                        tm
                }
            , Cmd.map TextareaMsg tc
            )

        TextClicked ->
            (model, Cmd.none)


subscriptions: Model -> Sub Msg
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

