module Models exposing(..)

import Entry as Entry exposing( Entry
                              , EntryStatus ( Active, Stopped )
                              , defaultEntry
                              , incrementEntryTime
                              , startEntry
                              , stopEntry)

import Dict exposing (Dict)


type alias Model =
    { newEntry : Entry
    , entries : Dict Int Entry
    }


initialModel =
    { newEntry = Entry.defaultEntry
    , entries = Dict.fromList []
    }


addNewEntryIfPossible model =
    let
        newEntry = model.newEntry
    in
        if String.isEmpty newEntry.title then
            model
        else
            let
                updatedEntries = Dict.insert newEntry.id newEntry model.entries
                updatedNewEntry = { newEntry |
                                        id = (Dict.size model.entries) + 1,
                                        title = "" }
            in
                { model |
                      entries = updatedEntries,
                      newEntry = updatedNewEntry
                }


updateEntry : (Entry -> Entry) -> Model -> Int -> Model
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
