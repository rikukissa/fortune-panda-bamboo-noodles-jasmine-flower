module Utils.HourMarking exposing (HourMarking, fromCSVRow)

import String exposing (split, padLeft, join, trim)
import List exposing (head, filter, length, map)
import Time.DateTime as DateTime exposing (DateTime, addDays, hour, fromISO8601)
import Ports exposing (CSVData)

type alias HourMarking =
  { personId : String
  , personName : String
  , start : DateTime
  , end : DateTime
  }

fromCSVRow : CSVData -> Result String HourMarking
fromCSVRow row =
  let
    values = split "," row |> map trim
    -- 26.3.2014 --> 2014-03-26
    formatDate = split "." >> List.reverse >> List.map (padLeft 2 '0') >> join "-"
    -- 2:00 --> 02:00
    formatTime = split ":" >> (List.map (padLeft 2 '0')) >> join ":"
  in
    case values of
      [personName, personId, dateString, startHour, endHour] ->
        let
          formattedDate = formatDate dateString
          formattedStart = formatTime startHour
          formattedEnd = formatTime endHour

          parsedStart = fromISO8601 (formattedDate ++ "T" ++ formattedStart ++ ":00Z")
          parsedEnd = fromISO8601 (formattedDate ++ "T" ++ formattedEnd ++ ":00Z")

          toHourMarking (start, end) = HourMarking personId personName start
            <| if (hour end) < (hour start) then (addDays 1 end) else end
        in
          (Result.map2 (,) parsedStart parsedEnd) |> Result.andThen (toHourMarking >> Ok)
      _ ->
        Err "Not enough columns in CSV row"
