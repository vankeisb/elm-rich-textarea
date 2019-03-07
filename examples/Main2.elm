module Main2 exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WithElmParser2
import WithPorts2


type alias Model =
    { pureModel : WithElmParser2.Model
    , portsModel : WithPorts2.Model
    }


type Msg
    = PureMsg WithElmParser2.Msg
    | PortsMsg WithPorts2.Msg


init =
    let
        ( pureModel, pureCmd ) =
            WithElmParser2.init "ta-pure"

        ( portsModel, portsCmd ) =
            WithPorts2.init "ta-ports"
    in
    ( { pureModel = pureModel
      , portsModel = portsModel
      }
    , Cmd.batch
        [ Cmd.map PureMsg pureCmd
        , Cmd.map PortsMsg portsCmd
        ]
    )


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Textarea... with style !" ]
        , p
            []
            [ text "Examples app for the "
            , a
                [ href "https://github.com/vankeisb/elm-rich-textarea" ]
                [ text "elm-rich-textarea" ]
            , text " Elm package."
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            ]
            [ WithElmParser2.view model.pureModel
                |> Html.map PureMsg
            , div
                [ style "width" "16px" ]
                []
            , WithPorts2.view model.portsModel
                |> Html.map PortsMsg
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PureMsg sub ->
            let
                ( m, c ) =
                    WithElmParser2.update sub model.pureModel
            in
            ( { model | pureModel = m }
            , Cmd.map PureMsg c
            )

        PortsMsg sub ->
            let
                ( m, c ) =
                    WithPorts2.update sub model.portsModel
            in
            ( { model | portsModel = m }
            , Cmd.map PortsMsg c
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WithElmParser2.subscriptions model.pureModel
            |> Sub.map PureMsg
        , WithPorts2.subscriptions model.portsModel
            |> Sub.map PortsMsg
        ]


main =
    Browser.element
        { init =
            \() ->
                init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
