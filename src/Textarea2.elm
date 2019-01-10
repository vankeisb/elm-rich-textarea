module Textarea2 exposing
    ( InitData
    , Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , defaultInitData
    , init
    , subscriptions
    , update
    , view
    )

import Html
import Range
import Task
import Textarea
import Tuple


type Msg s m
    = LiftMsg Textarea.Msg
    | ApplyStyles Textarea.HighlightId (List ( Range.Range, s ))


type alias ApplyStylesFun s m =
    List ( Range.Range, s ) -> m


type alias InitData =
    Textarea.InitData


type alias Model s p =
    Textarea.Model s p


init : InitData -> ( Model s p, Cmd (Msg s m) )
init =
    Textarea.init >> Tuple.mapSecond (Cmd.map LiftMsg)


defaultInitData =
    Textarea.defaultInitData


type alias ViewConfig s p m =
    { lift : Msg s m -> m
    , highlighter : Textarea.Highlighter s m
    , predictionConfig : Maybe (Textarea.PredictionConfig p m)
    }


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
    { lift : Msg s m -> m
    , highlighter : Textarea.Highlighter s m
    , predictionConfig : Maybe (Textarea.PredictionConfig p m)
    , requestHighlight : ApplyStylesFun s m -> String -> m
    }


update : UpdateConfig s p m -> Msg s m -> Model s p -> ( Model s p, Cmd m )
update config msg model =
    let
        config_ =
            { lift = config.lift << LiftMsg
            , highlighter = always [] -- TODO remove from Textarea.Config for update
            , predictionConfig = config.predictionConfig
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


subscriptions : Model s m -> Sub (Msg s m)
subscriptions model =
    Sub.map LiftMsg <| Textarea.subscriptions model
