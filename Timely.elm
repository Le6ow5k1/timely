port module Timely exposing (..)

import Models exposing(Model, initialModel, toJson, moveEntry)
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
        | RemoveEntry Entry.Id
        | MoveEntry Entry.Id Entry.Position
        | ToggleIsEditing Entry.Id
        | ChangeEntryTitle Entry.Id String
        | Start Entry.Id
        | Stop Entry.Id
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
            if String.isEmpty model.newEntry.title then
                ( model, Cmd.none )
            else
                let
                    startCurrentEntryStopOthers = \currentId id entry ->
                                                  if id == currentId then
                                                      Entry.start entry
                                                  else
                                                      Entry.stop entry
                    modelWithAddedEntry = Models.addNewEntry model
                    updatedModel = Models.updateEntries (startCurrentEntryStopOthers model.newEntry.id) modelWithAddedEntry
                in
                    ( updatedModel, Cmd.none )

        RemoveEntry id ->
            let
                updatedEntries = Dict.remove id model.entries
            in
                ( { model | entries = updatedEntries } , Cmd.none )

        MoveEntry id newPosition ->
            ( Models.moveEntry model id newPosition , Cmd.none )

        ToggleIsEditing id ->
            let
                updatedModel = (Models.updateEntry (\e -> { e | isEditing = not e.isEditing }) model id)
            in
                ( updatedModel, Cmd.none )

        ChangeEntryTitle id newTitle ->
            let
                updatedModel = (Models.updateEntry (\e -> { e | title = newTitle }) model id)
            in
                ( updatedModel, Cmd.none )

        Start id ->
            let
                startCurrentEntryStopOthers = \currentId entry ->
                                              if currentId == id then
                                                  Entry.start entry
                                              else
                                                  Entry.stop entry
                updatedModel = Models.updateEntries startCurrentEntryStopOthers model
            in
                ( updatedModel, Cmd.none )

        Stop id ->
            let
                updatedModel = (Models.updateEntry Entry.stop model id)
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
              , style [ ("font-size", "18px") ]
              ]
        []
    else
        h4 [ style [ ("cursor", "pointer") ], onClick (ToggleIsEditing entry.id) ]
            [ text entry.title ]


entryView : Entry -> Html Msg
entryView entry =
    li [ class "list-group-item" ]
        [ div [ class "row" ]
              [ div [ class "col-xs-10 col-sm-10 col-md-10" ]
                    [ (entryTitleView entry) ]
              , div [ class "col-xs-2 col-sm-2 col-md-2" ]
                  [ button [ class "btn btn-link text-muted pull-right", onClick (RemoveEntry entry.id) ]
                        [ span [ class "glyphicon glyphicon-trash" ]
                              []
                        ]
                  ]
              ]
        , div [ class "row" ]
            [ div [ class "col-xs-1 col-sm-1 col-md-1" ]
                  [ h4 []
                        [ span [ class "glyphicon glyphicon-th moving-handle text-muted small" ]
                              []
                        ]
                  ]
            , div [ class "col-xs-11 col-sm-11 col-md-11" ]
                [ h4 []
                  [ span [ class "glyphicon glyphicon-time", style [ ("margin-right", "0.3em") ] ]
                        []
                  , span [ style [ ("margin-right", "1em") ] ]
                      [ text (formatTime entry.time) ]
                  , (startStopButtonView entry)
                  ]
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
        , ul [ class "row list-group entries-list", id "entries-list" ]
            (model.entries |> Dict.values |> (List.sortBy (\e -> -e.id)) |> (List.map entryView))
        , (totalTimeView model)
        ]
