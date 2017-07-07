module Models exposing(..)

import Entry as Entry exposing( Entry
                              , EntryStatus ( Active, Stopped )
                              )

import Dict exposing (Dict)
import Json.Encode
import Json.Decode


type alias Model =
    { newEntry : Entry
    , entries : Dict Int Entry
    }


initialModel : Model
initialModel =
    { newEntry = Entry.default
    , entries = Dict.fromList []
    }


toJson : Model -> String
toJson model =
    let
        entries = model.entries |> Dict.values |> (List.map Entry.toJsonValue)
        modelObject = Json.Encode.object [ ("newEntry", (model.newEntry |> Entry.toJsonValue))
                                         , ("entries", (Json.Encode.list entries))
                                         ]
    in
        Json.Encode.encode 0 modelObject


addNewEntry : Model -> Model
addNewEntry model =
    let
        newEntry = model.newEntry
    in
        let
            updatedEntries = Dict.insert newEntry.id newEntry model.entries
            updatedNewEntry = { newEntry |
                                    id = newEntry.id + 1,
                                    position = newEntry.position + 1,
                                    title = "" }
        in
            { model |
                  entries = updatedEntries,
                  newEntry = updatedNewEntry
            }


moveEntry : Model -> Entry.Id -> Entry.Position -> Model
moveEntry model entryId newPosition =
    let
        moveEntry = \currentId id entry ->
                    if id == currentId then
                        { entry | position = newPosition }
                    else
                        if entry.position >= newPosition then
                            { entry | position = entry.position + 1 }
                        else
                            entry
    in
        updateEntries (moveEntry entryId) model


updateEntry : (Entry -> Entry) -> Model -> Entry.Id -> Model
updateEntry updateFn model entryId =
    let
        updatedEntries = Dict.update entryId (updateEntryHelper updateFn) model.entries
    in
        { model | entries = updatedEntries }


updateEntryHelper : (Entry -> Entry) -> Maybe Entry -> Maybe Entry
updateEntryHelper updateFn entry =
    case entry of
        Just entry -> Just (updateFn entry)

        Nothing -> Nothing


updateEntries updateFn model =
    let
        updatedEntries = Dict.map updateFn model.entries
    in
        { model | entries = updatedEntries }
