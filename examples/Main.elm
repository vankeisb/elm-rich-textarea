port module Main exposing (Model, Msg(..), MyStyle(..), highlighter, init, main, renderer, subscriptions, update, view)

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
    | DelayHighlight (Textarea.ReturnStyles Msg MyStyle) (List ( Range, MyStyle ))
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
                , highlighter = highlighter
                , initialText = "let\n  foo = 1\nin\n  foo + bar"
                , lift = TextareaMsg
                }
    in
    ( { textareaModel = m
      , returnHighlight = Nothing
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
                (Textarea.attributedRenderer model.textareaModel renderer)
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
                    { onHighlight = onHighlight
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

        DelayHighlight return list ->
            if model.returnHighlight == Nothing then
                ( { model | returnHighlight = Just return }
                , delayHighlight list
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
        [ delayed (UpdateHighlight << decodeHighlight)
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


onHighlight : Textarea.ReturnStyles Msg MyStyle -> String -> Cmd Msg
onHighlight return text =
    Task.succeed (highlighter text)
        |> Task.perform (DelayHighlight return)


delayHighlight : List ( Range, MyStyle ) -> Cmd Msg
delayHighlight list =
    list
        |> encodeHighlight
        |> delay


port delay : E.Value -> Cmd msg


port delayed : (E.Value -> msg) -> Sub msg


decodeHighlight : E.Value -> List ( Range, MyStyle )
decodeHighlight value =
    value
        |> D.decodeValue highlightDecoder
        |> Result.withDefault []


encodeHighlight : List ( Range, MyStyle ) -> E.Value
encodeHighlight list =
    list
        |> E.list
            (\( range, mystyle ) ->
                E.object
                    [ ( "range", encodeRange range )
                    , ( "mystyle", encodeMyStyle mystyle )
                    ]
            )


highlightDecoder : D.Decoder (List ( Range, MyStyle ))
highlightDecoder =
    D.list <|
        D.map2 Tuple.pair
            (D.field "range" rangeDecoder)
            (D.field "mystyle" myStyleDecoder)


rangeDecoder : D.Decoder Range
rangeDecoder =
    D.map2 Range.range (D.index 0 D.int) (D.index 1 D.int)


encodeRange : Range -> E.Value
encodeRange range =
    E.list E.int <| tupleAsList <| Range.getBounds range


tupleAsList : ( a, a ) -> List a
tupleAsList ( a, b ) =
    [ a, b ]


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


encodeMyStyle : MyStyle -> E.Value
encodeMyStyle style =
    case style of
        Keyword ->
            E.string "Keyword"

        Identifier ->
            E.string "Identifier"
