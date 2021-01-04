module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, h3, img, input, label, text)
import Html.Attributes exposing (class, for, id, src, type_)
import Html.Events exposing (onInput, onSubmit)
import Process
import Task



---- MODEL ----


type alias Answer =
    String


type alias Mission =
    { hint : String
    , answers : List Answer
    , position : Int
    }


type NotificationLevel
    = Success
    | Error


type alias Notification =
    { text : String
    , level : NotificationLevel
    }


type alias Model =
    { missions : List Mission
    , currentMission : Maybe Mission
    , answer : Answer
    , notifications : List Notification
    }


init : ( Model, Cmd Msg )
init =
    ( { missions =
            [ { hint = "Eka kyssäri"
              , answers = [ "Eka vastaus" ]
              , position = 0
              }
            , { hint = "Toka kyssäri"
              , answers = [ "Toka vastaus a", "Toka vastaus b" ]
              , position = 1
              }
            ]
      , currentMission =
            Just
                { hint = "Eka kyssäri"
                , answers = [ "Eka vastaus" ]
                , position = 0
                }
      , answer = ""
      , notifications = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateAnswer String
    | SubmitAnswer
    | ClearNotifications


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnswer newAnswer ->
            ( { model | answer = newAnswer }, Cmd.none )

        SubmitAnswer ->
            case model.currentMission of
                Nothing ->
                    ( model, Cmd.none )

                Just currentMission ->
                    let
                        normalizedAnswer =
                            normalizeAnswer model.answer

                        normalizedCorrectAnswers =
                            List.map normalizeAnswer currentMission.answers
                    in
                    case List.member normalizedAnswer normalizedCorrectAnswers of
                        True ->
                            let
                                newMission =
                                    findNextMission model.missions currentMission
                            in
                            ( { model | notifications = [ { level = Success, text = "Oikein!" } ], currentMission = newMission }, setClearNotifications )

                        False ->
                            ( { model | notifications = [ { level = Error, text = "Väärin!" } ] }, setClearNotifications )

        ClearNotifications ->
            ( { model | notifications = [] }, Cmd.none )



---- HELPERS ----


findNextMission : List Mission -> Mission -> Maybe Mission
findNextMission missions currentMission =
    List.head (List.filter (\m -> m.position == (1 + currentMission.position)) missions)


normalizeAnswer : String -> String
normalizeAnswer =
    String.toLower >> String.trim


setClearNotifications : Cmd Msg
setClearNotifications =
    Process.sleep 5000
        |> Task.perform (\_ -> ClearNotifications)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewNotifications model.notifications
        , viewCurrentMission model.currentMission model.answer
        ]


viewNotifications : List Notification -> Html Msg
viewNotifications notifications =
    case List.isEmpty notifications of
        True ->
            Html.text ""

        False ->
            div [ class "notifications-wrapper" ] (List.map viewNotification notifications)


viewNotification : Notification -> Html msg
viewNotification notification =
    let
        color =
            case notification.level of
                Success ->
                    "success"

                Error ->
                    "danger"
    in
    div [ class ("alert alert-" ++ color) ] [ text notification.text ]


viewCurrentMission : Maybe Mission -> Answer -> Html Msg
viewCurrentMission maybeMission answer =
    case maybeMission of
        Just mission ->
            viewMission mission answer

        Nothing ->
            noMissions


noMissions : Html Msg
noMissions =
    div [] [ text "Vihjettä ei löytynyt. Ota yhteyttä tekniseen tukeen WhatsApissa." ]


viewMission : Mission -> Answer -> Html Msg
viewMission mission answer =
    div []
        [ h3 [] [ text mission.hint ]
        , form
            [ onSubmit SubmitAnswer ]
            [ div
                [ class "form-group" ]
                [ label [ for "hint" ] [ text "Vastaus" ]
                , input [ type_ "text", class "form-control", id "hint", onInput UpdateAnswer ] [ text answer ]
                ]
            , button [ type_ "submit", class "btn btn-primary" ] [ text "Lähetä" ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
