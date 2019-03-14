module Textarea2 exposing
    ( ApplyPredictionsFun
    , ApplyStylesFun
    , InitData
    , Model
    , Msg
    , PredictionConfig
    , PredictionConfig2
    , UpdateConfig
    , ViewConfig
    , defaultInitData
    , init
    , predictionsDecoder
    , stylesDecoder
    , subscriptions
    , update
    , view
    )

import Html
import Json.Decode as D
import Range
import Task
import Textarea
import Tuple


type Msg s p m
    = LiftMsg Textarea.Msg
    | ApplyStyles Textarea.HighlightId (List ( Range.Range, s ))
    | ApplyPredictions Textarea.PredictionId (List p)


type alias ApplyStylesFun s m =
    List ( Range.Range, s ) -> m


type alias ApplyPredictionsFun p m =
    List p -> m


type alias InitData =
    Textarea.InitData


type alias Model s p =
    Textarea.Model s p


init : InitData -> ( Model s p, Cmd (Msg s p m) )
init =
    Textarea.init >> Tuple.mapSecond (Cmd.map LiftMsg)


defaultInitData =
    Textarea.defaultInitData


type alias ViewConfig s p m =
    { lift : Msg s p m -> m
    , highlighter : Textarea.Highlighter s m
    , predictionConfig : Maybe (Textarea.PredictionConfig p m)
    }


type alias PredictionConfig p m =
    Textarea.PredictionConfig p m


view : ViewConfig s p m -> Model s p -> Html.Html m
view config model =
    let
        config_ =
            { lift = config.lift << LiftMsg
            , highlighter = config.highlighter
            , predictionConfig = config.predictionConfig
            }
    in
    Textarea.view config_ model


type alias UpdateConfig s p m =
    { lift : Msg s p m -> m
    , predictionConfig : Maybe (PredictionConfig2 p m)
    , requestHighlight : ApplyStylesFun s m -> String -> m
    }


type alias PredictionConfig2 p m =
    { config : Textarea.PredictionConfig p m
    , requestPrediction : ApplyPredictionsFun p m -> ( String, Int ) -> m
    }


update : UpdateConfig s p m -> Msg s p m -> Model s p -> ( Model s p, Cmd m )
update config msg model =
    let
        config_ =
            { lift = config.lift << LiftMsg
            , highlighter = always [] -- TODO remove from Textarea.Config for update
            , predictionConfig = Maybe.map .config config.predictionConfig
            }
    in
    case msg of
        LiftMsg sub ->
            let
                ( model1, cmd, o ) =
                    Textarea.update config_ sub model

                cmd1 =
                    case o of
                        Just (Textarea.RequestHighlight hr) ->
                            let
                                applyStyles =
                                    config.lift << ApplyStyles hr.id

                                requestHighlight =
                                    config.requestHighlight applyStyles
                            in
                            hr.text
                                |> Task.succeed
                                |> Task.perform requestHighlight

                        Just (Textarea.RequestPrediction pr) ->
                            config.predictionConfig
                                |> Maybe.map (requestPredictionCmd config.lift pr)
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none
            in
            ( model1
            , Cmd.batch
                [ Cmd.map (config.lift << LiftMsg) cmd
                , cmd1
                ]
            )

        ApplyStyles id styles ->
            let
                model1 =
                    Textarea.applyStyles
                        id
                        styles
                        model
            in
            ( model1
            , Cmd.none
            )

        ApplyPredictions id predictions ->
            Textarea.applyPredictions
                config_
                { id = id, predictions = predictions }
                model
                |> Tuple.mapSecond (Cmd.map (config.lift << LiftMsg))


requestPredictionCmd : (Msg s p m -> m) -> Textarea.PredictionRequest -> PredictionConfig2 p m -> Cmd m
requestPredictionCmd lift pr config =
    let
        applyPredictions =
            lift << ApplyPredictions pr.id

        requestPrediction =
            config.requestPrediction applyPredictions
    in
    ( pr.text, pr.offset )
        |> Task.succeed
        |> Task.perform requestPrediction


subscriptions : Model s p -> Sub (Msg s p m)
subscriptions model =
    Sub.map LiftMsg <| Textarea.subscriptions model


stylesDecoder : D.Decoder s -> D.Decoder (List ( Range.Range, s ))
stylesDecoder styleDecoder =
    let
        rangeAndStyleDecoder : D.Decoder ( Range.Range, s )
        rangeAndStyleDecoder =
            D.map2
                (\range styleValue ->
                    ( range, styleValue )
                )
                (D.field "range" Textarea.rangeDecoder)
                (D.field "style" styleDecoder)
    in
    D.list rangeAndStyleDecoder


predictionsDecoder : D.Decoder p -> D.Decoder (List p)
predictionsDecoder predictionDecoder =
    D.list predictionDecoder
