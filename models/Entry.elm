module Entry exposing ( Entry
                      , EntryStatus ( Active, Stopped )
                      , defaultEntry
                      , incrementEntryTime
                      , startEntry
                      , stopEntry
                      )

import Time exposing (Time)


type EntryStatus =
    Active | Stopped


type alias Entry =
    { id : Int
    , title : String
    , time : Time
    , status : EntryStatus
    , isEditing : Bool
    }


defaultEntry =
    Entry 0 "" 0 Stopped False


incrementEntryTime : Entry -> Float -> Entry
incrementEntryTime entry by =
    case entry.status of
        Active -> { entry | time = entry.time + (by * 1000) }

        Stopped -> entry


startEntry entry =
    { entry | status = Active }


stopEntry entry =
    { entry | status = Stopped }
