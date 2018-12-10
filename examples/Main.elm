port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Platform.Cmd exposing (Cmd)
import Range exposing (Range)
import Task
import Textarea


type Msg
    = TextareaMsg Textarea.Msg
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
        initialText =
            "let\n  foo = 1\nin\n  foo + bar"

        ( m, c ) =
            Textarea.init
                { idPrefix = "my-ta"
                , initialText = initialText
                , initialStyles = highlight initialText
                , debounceMs = 1000
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


highlight : String -> List ( Range, MyStyle )
highlight text =
    let
        x =
            Debug.log "hlText" text

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
                ( tm, c, o ) =
                    Textarea.update sub model.textareaModel

                tm2 =
                    case o of
                        Just (Textarea.RequestHighlight hr) ->
                            Textarea.applyStyles
                                hr.id
                                (highlight hr.text)
                                tm

                        Nothing ->
                            tm
            in
            ( { model
                | textareaModel =
                    tm2
              }
            , Cmd.map TextareaMsg c
            )

        TextClicked ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TextareaMsg <| Textarea.subscriptions model.textareaModel
        ]


main =
    Browser.element
        { init =
            \() ->
                init "my-textarea"
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

fromString : String -> MyStyle
fromString style =
    case style of
        "Keyword" ->
            Keyword

        "Identifier" ->
            Identifier

        _ ->
            Identifier
