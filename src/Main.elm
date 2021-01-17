port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, h3, img, input, label, text)
import Html.Attributes exposing (class, for, id, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Process
import Task



---- MODEL ----


type alias Progression =
    List Int


type alias Mission =
    { hint : String
    , answers : List String
    , position : Int
    }


type NotificationLevel
    = Success
    | Error


type alias Notification =
    { text : String
    , level : NotificationLevel
    }


type alias ContentfulSettings =
    { spaceId : String
    , accessKey : String
    }


type alias Model =
    { missions : List Mission
    , currentMission : Maybe Mission
    , progression : Progression
    , answerField : String
    , notifications : List Notification
    }


type alias Flags =
    { contentfulSpaceId : String
    , contentfulAccessKey : String
    , progression : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { missions = []
      , currentMission = Nothing
      , progression = decodeProgress flags.progression
      , answerField = ""
      , notifications = []
      }
    , getMissions flags.contentfulSpaceId flags.contentfulAccessKey
    )



---- UPDATE ----


type Msg
    = UpdateAnswer String
    | SubmitAnswer
    | ClearNotifications
    | GotMissions (Result Http.Error (List Mission))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnswer newAnswer ->
            ( { model | answerField = newAnswer }, Cmd.none )

        SubmitAnswer ->
            case model.currentMission of
                Nothing ->
                    ( model, Cmd.none )

                Just currentMission ->
                    let
                        normalizedAnswer =
                            normalizeAnswer model.answerField

                        normalizedCorrectAnswers =
                            List.map normalizeAnswer currentMission.answers
                    in
                    case List.member normalizedAnswer normalizedCorrectAnswers of
                        True ->
                            let
                                newProgression =
                                    case model.currentMission of
                                        Just m ->
                                            m.position :: model.progression

                                        Nothing ->
                                            model.progression

                                newMission =
                                    findNextMission model.missions newProgression
                            in
                            ( { model
                                | notifications = [ { level = Success, text = "Oikein!" } ]
                                , currentMission = newMission
                                , progression = newProgression
                                , answerField = ""
                              }
                            , Cmd.batch [ setClearNotifications, saveProgress newProgression ]
                            )

                        False ->
                            ( { model
                                | notifications = [ { level = Error, text = "Väärin!" } ]
                                , answerField = ""
                              }
                            , setClearNotifications
                            )

        ClearNotifications ->
            ( { model | notifications = [] }, Cmd.none )

        GotMissions response ->
            case response of
                Ok missionsList ->
                    let
                        maybeFirstMission =
                            findNextMission missionsList model.progression
                    in
                    ( { model | missions = missionsList, currentMission = maybeFirstMission }, Cmd.none )

                Err _ ->
                    ( { model | notifications = [ { level = Error, text = "Tehtäviä ei saatu haettua. Ota yhteyttä tekniseen tukeen." } ] }, Cmd.none )



---- HTTP ----


getMissions : String -> String -> Cmd Msg
getMissions spaceId accessKey =
    Http.get
        { url = "https://cdn.contentful.com/spaces/" ++ spaceId ++ "/environments/master-2021-01-05/entries?access_token=" ++ accessKey ++ "&content_type=missions"
        , expect = Http.expectJson GotMissions missionsDecoder
        }


missionDecoder : D.Decoder Mission
missionDecoder =
    D.map3 Mission
        (D.field "hint" D.string)
        (D.field "answers" (D.list D.string))
        (D.field "position" D.int)


missionsDecoder : D.Decoder (List Mission)
missionsDecoder =
    D.field "items" (D.list (D.field "fields" missionDecoder))



---- HELPERS ----


missionIsCompleted : Progression -> Mission -> Bool
missionIsCompleted progression mission =
    List.member mission.position progression
        |> not


findNextMission : List Mission -> Progression -> Maybe Mission
findNextMission missions progression =
    let
        availableMissions =
            List.filter (missionIsCompleted progression) missions
                |> List.sortBy .position
    in
    List.head availableMissions


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
        , viewCurrentMission model.currentMission ((List.isEmpty >> not) model.missions) model.answerField
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


type alias MissionsExist =
    Bool


viewCurrentMission : Maybe Mission -> MissionsExist -> String -> Html Msg
viewCurrentMission maybeMission missionsExist answerField =
    case maybeMission of
        Just mission ->
            viewMission mission answerField

        Nothing ->
            noDisplayableMissions missionsExist


noDisplayableMissions : MissionsExist -> Html Msg
noDisplayableMissions missionsExist =
    case missionsExist of
        True ->
            div [] [ text "Kaikki tehtävät ovat tehdyt. Onneksi olkoon!" ]

        False ->
            div [] [ text "Vihjettä ei löytynyt. Ota yhteyttä tekniseen tukeen WhatsApissa." ]


viewMission : Mission -> String -> Html Msg
viewMission mission answerField =
    div []
        [ h3 [] [ text mission.hint ]
        , form
            [ onSubmit SubmitAnswer ]
            [ div
                [ class "form-group" ]
                [ label [ for "hint" ] [ text "Vastaus" ]
                , input [ type_ "text", class "form-control", id "hint", onInput UpdateAnswer, value answerField ] []
                ]
            , button [ type_ "submit", class "btn btn-primary" ] [ text "Lähetä" ]
            ]
        ]



-- PORTS ----


port saveProgressToLocalStore : String -> Cmd msg


decodeProgress : String -> List Int
decodeProgress progression =
    case D.decodeString (D.list D.int) progression of
        Ok res ->
            res

        Err _ ->
            []


saveProgress : List Int -> Cmd msg
saveProgress ids =
    E.list E.int ids
        |> E.encode 0
        |> saveProgressToLocalStore



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
