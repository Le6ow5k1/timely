port module Timely exposing (..)

import Models exposing(Model, initialModel, toJson)
import Entry exposing(Entry, EntryStatus ( Active, Stopped ))
import Utils exposing(formatTime)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Time exposing (Time, second)
import Dict


main =
    Html.program
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


port setStorage : String -> Cmd msg


updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage (Models.toJson newModel), cmds ]
    )


init : (Model, Cmd Msg)
init =
    (
     initialModel
    , Cmd.none
    )


type Msg =
    ChangeNewEntry String
        | AddEntry
        | RemoveEntry Int
        | ToggleIsEditing Int
        | ChangeEntryTitle Int String
        | Start Int
        | Stop Int
        | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeNewEntry newTitle ->
            let
                newEntry = model.newEntry
                updatedNewEntry = { newEntry | title = newTitle }
                updatedModel = { model | newEntry = updatedNewEntry }
            in
                ( updatedModel, Cmd.none )

        AddEntry ->
            let
                startCurrentEntryStopOthers = \currentId id entry ->
                                              if id == currentId then
                                                  Entry.start entry
                                              else
                                                  Entry.stop entry
                modelWithAddedEntry = Models.addNewEntryIfPossible model
                updatedModel = Models.updateEntries (startCurrentEntryStopOthers model.newEntry.id) modelWithAddedEntry
            in
                ( updatedModel, Cmd.none )

        RemoveEntry entryId ->
            let
                updatedEntries = Dict.remove entryId model.entries
            in
                ( { model | entries = updatedEntries } , Cmd.none )

        ToggleIsEditing entryId ->
            let
                updatedModel = (Models.updateEntry (\e -> { e | isEditing = not e.isEditing }) model entryId)
            in
                ( updatedModel, Cmd.none )

        ChangeEntryTitle entryId newTitle ->
            let
                updatedModel = (Models.updateEntry (\e -> { e | title = newTitle }) model entryId)
            in
                ( updatedModel, Cmd.none )

        Start entryId ->
            let
                startCurrentEntryStopOthers = \id entry ->
                                              if id == entryId then
                                                  Entry.start entry
                                              else
                                                  Entry.stop entry
                updatedModel = Models.updateEntries startCurrentEntryStopOthers model
            in
                ( updatedModel, Cmd.none )

        Stop entryId ->
            let
                updatedModel = (Models.updateEntry Entry.stop model entryId)
            in
                ( updatedModel, Cmd.none )

        Tick newTime ->
            let
                updatedModel = Models.updateEntries (\id entry -> (Entry.incrementTime entry 1)) model
            in
                ( updatedModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


startStopButtonView : Entry -> Html Msg
startStopButtonView entry =
    case entry.status of
        Active ->
            button [ onClick (Stop entry.id), class "btn btn-default btn-sm" ]
                [ span [ class "glyphicon glyphicon-pause", style [ ("margin-right", "0.3em") ] ]
                      []
                 , text "Stop" ]
        Stopped ->
            button [ onClick (Start entry.id), class "btn btn-primary btn-sm" ]
                [ span [ class "glyphicon glyphicon-play", style [ ("margin-right", "0.3em") ] ]
                      []
                 , text "Start" ]


entryTitleView : Entry -> Html Msg
entryTitleView entry =
    if entry.isEditing then
        input [ class "form-control"
              , type_ "text"
              , onInput (ChangeEntryTitle entry.id)
              , onBlur (ToggleIsEditing entry.id)
              , value entry.title
              , disabled (not entry.isEditing)
              ]
        []
    else
        h5 [ style [ ("cursor", "pointer") ], onClick (ToggleIsEditing entry.id) ]
            [ text entry.title ]


entryView : Entry -> Html Msg
entryView entry =
    li [ class "list-group-item" ]
        [ (entryTitleView entry)
        , button [ class "btn btn-link pull-right text-muted", onClick (RemoveEntry entry.id) ]
            [ span [ class "glyphicon glyphicon-trash" ]
                  []
            ]

        , div [ class "row" ]
            [ h4 []
                  [ span [ class "glyphicon glyphicon-time", style [ ("margin-right", "0.3em") ] ]
                        []
                  , span [ style [ ("margin-right", "1em") ] ]
                      [ text (formatTime entry.time) ]
                  , (startStopButtonView entry)
                  ]
            ]
        ]


newEntryView : Entry -> Html Msg
newEntryView newEntry =
    div [ class "form-inline" ]
        [ div [ class "form-group" ]
              [ input [ class "form-control"
                      , type_ "text"
                      , placeholder "Your task"
                      , onInput ChangeNewEntry
                      , value newEntry.title
                      , size 20
                      ]
                    []
              ]
        , button [ type_ "submit", class "btn btn-primary pull-right", onClick AddEntry ]
            [ text "Add" ]
        ]


totalTimeView model =
    let
        totalTime = Dict.foldl (\id e acc -> acc + e.time) 0 model.entries
        totalTimeText = "Total: " ++ (formatTime totalTime)
    in
        ul [ class "row list-group" ]
            [ li [ class "list-group-item" ]
                  [ text totalTimeText ]
            ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
              [ (newEntryView model.newEntry) ]
        , ul [ class "row list-group" ]
            (model.entries |> Dict.values |> (List.map entryView))
        , (totalTimeView model)
        ]
