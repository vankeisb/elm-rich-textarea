module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WithElmParser
import WithPorts

type alias Model =
    { pureModel: WithElmParser.Model
    , portsModel: WithPorts.Model
    }



type Msg
    = PureMsg WithElmParser.Msg
    | PortsMsg WithPorts.Msg


init =
    let
        (pureModel, pureCmd) =
            WithElmParser.init "ta-pure"

        (portsModel, portsCmd) =
            WithPorts.init "ta-ports"

    in
    (
        { pureModel = pureModel
        , portsModel = portsModel
        }
    , Cmd.batch
        [ Cmd.map PureMsg pureCmd
        , Cmd.map PortsMsg portsCmd
        ]
    )


view: Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Textarea... with style !" ]
        , h2
            []
            [ text "Pure Elm" ]
        , WithElmParser.view model.pureModel
            |> Html.map PureMsg
        , h2
            []
            [ text "With ports"]
        , WithPorts.view model.portsModel
            |> Html.map PortsMsg
        ]


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PureMsg sub ->
            let
               (m,c) =
                   WithElmParser.update sub model.pureModel
            in
            ( { model | pureModel = m }
            , Cmd.map PureMsg c
            )

        PortsMsg sub ->
            let
               (m,c) =
                   WithPorts.update sub model.portsModel
            in
            ( { model | portsModel = m }
            , Cmd.map PortsMsg c
            )


subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WithElmParser.subscriptions model.pureModel
            |> Sub.map PureMsg
        , WithPorts.subscriptions model.portsModel
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