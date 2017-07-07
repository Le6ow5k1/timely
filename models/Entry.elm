module Entry exposing ( Entry
                      , EntryStatus ( Active, Stopped )
                      , Id
                      , Position
                      , default
                      , incrementTime
                      , start
                      , stop
                      , toJsonValue
                      )

import Time exposing (Time)
import Json.Encode
import Json.Decode


type EntryStatus =
    Active | Stopped

type alias Id = Int

type alias Position = Int


type alias Entry =
    { id : Id
    , title : String
    , time : Time
    , status : EntryStatus
    , isEditing : Bool
    , position : Position
    }


toJsonValue : Entry -> Json.Encode.Value
toJsonValue { id, title, time, status, isEditing } =
        Json.Encode.object [ ("id", Json.Encode.int id)
                           , ("title", Json.Encode.string title)
                           , ("time", (time |> Time.inMilliseconds |> Json.Encode.float))
                           , ("status", (status |> toString |> Json.Encode.string))
                           , ("isEditing", Json.Encode.bool isEditing)
                           ]


default =
    Entry 0 "" 0 Stopped False 1


incrementTime : Entry -> Float -> Entry
incrementTime entry by =
    case entry.status of
        Active -> { entry | time = entry.time + (by * 1000) }

        Stopped -> entry


start entry =
    { entry | status = Active }


stop entry =
    { entry | status = Stopped }
