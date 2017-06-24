module Utils exposing(formatTime)

import Time exposing(Time)
import String


formatTime : Time -> String
formatTime time =
    let
        hours = truncate (time / Time.hour)
        minutes = truncate ((time - ((toFloat hours) * Time.hour)) / Time.minute)
        seconds = truncate ((time - ((toFloat hours) * Time.hour) - ((toFloat minutes) * Time.minute)) / Time.second)
        formattedHours = hours |> toString |> twoDigitsFormat
        formattedMinutes = minutes |> toString |> twoDigitsFormat
        formattedSeconds = seconds |> toString |> twoDigitsFormat
    in
        if hours == 0 then
            formattedMinutes ++ ":" ++ formattedSeconds
        else
            formattedHours ++ ":" ++ formattedMinutes ++ ":" ++ formattedSeconds


twoDigitsFormat string =
    String.padLeft 2 '0' string
