port module Main exposing
    ( Model
    , Msg(..)
    , MyStyle(..)
    , init
    , main
    , resolveStyles
    , subscriptions
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Platform.Cmd exposing (Cmd)
import Range exposing (Range)
import Styles
import Task
import Textarea


type Msg
    = TextareaMsg (Textarea.Msg MyStyle)
    | TextClicked
    | OnHighlight (Textarea.ReturnStyles Msg MyStyle) String
    | UpdateHighlight (List ( Range, MyStyle ))


type MyStyle
    = Keyword
    | Identifier


type alias Model =
    { textareaModel : Textarea.Model Msg MyStyle
    , returnHighlight : Maybe (List ( Range, MyStyle ) -> Cmd Msg)
    }


init : String -> ( Model, Cmd Msg )
init idPrefix =
    let
        ( m, c ) =
            Textarea.init
                { idPrefix = "my-ta"
                , initialText = "let\n  foo = 1\nin\n  foo + bar"
                , lift = TextareaMsg
                , resolveStyles = resolveStyles
                }
    in
    ( { textareaModel = m
      , returnHighlight = Nothing
      }
    , c
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
            [ Textarea.view model.textareaModel
            ]
        ]


resolveStyles : List MyStyle -> List (Html.Attribute Msg)
resolveStyles myStyles =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextareaMsg sub ->
            let
                ( tm, c ) =
                    Textarea.update onHighlight sub model.textareaModel
            in
            ( { model
                | textareaModel =
                    tm
              }
            , c
            )

        TextClicked ->
            ( model, Cmd.none )

        OnHighlight return text ->
            if model.returnHighlight == Nothing then
                ( { model | returnHighlight = Just return }
                , text
                    |> E.string
                    |> requestHighlight
                )

            else
                ( model, Cmd.none )

        UpdateHighlight list ->
            case model.returnHighlight of
                Just return ->
                    ( { model | returnHighlight = Nothing }
                    , return list
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ responseHighlight (UpdateHighlight << decodeHighlight)
        , Sub.map TextareaMsg <| Textarea.subscriptions model.textareaModel
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


onHighlight : Textarea.OnHighlight Msg MyStyle
onHighlight return text =
    Task.perform (OnHighlight return) <| Task.succeed text


port requestHighlight : E.Value -> Cmd msg


port responseHighlight : (E.Value -> msg) -> Sub msg


decodeHighlight : E.Value -> List ( Range, MyStyle )
decodeHighlight value =
    value
        |> D.decodeValue highlightDecoder
        |> Result.withDefault []


highlightDecoder : D.Decoder (List ( Range, MyStyle ))
highlightDecoder =
    D.list <|
        D.map2 Tuple.pair
            (D.field "range" rangeDecoder)
            (D.field "mystyle" myStyleDecoder)


rangeDecoder : D.Decoder Range
rangeDecoder =
    D.map2 Range.range (D.index 0 D.int) (D.index 1 D.int)


myStyleDecoder : D.Decoder MyStyle
myStyleDecoder =
    D.map fromString D.string


fromString : String -> MyStyle
fromString style =
    case style of
        "Keyword" ->
            Keyword

        "Identifier" ->
            Identifier

        _ ->
            Identifier
