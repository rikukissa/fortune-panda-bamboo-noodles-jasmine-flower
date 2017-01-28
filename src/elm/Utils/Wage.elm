module Utils.Wage exposing (Wage, HourMarking, calculateWages, fromCSVRow)

import Maybe exposing (withDefault)
import String exposing (split, padLeft, join, trim)
import List exposing (head, filter, length, map, foldl, range, sortBy)
import Guards exposing ((|=),(=>))
import List.Extra exposing (groupWhile)
import Time.DateTime as DateTime exposing (DateTime, addDays, dateTime, delta, hour, fromISO8601)
import Ports exposing (CSVData)

workdayLength = 8
hourlyWage = 3.75

type alias Wage =
  { personId : String
  , regular : Float
  , evening : Float
  , overtime : Float
  }

-- TODO move to separate file
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


getDeltaHours : DateTime -> DateTime -> Int
getDeltaHours start end =
  delta end start |> .hours

calculateEveningCompensation : DateTime -> DateTime -> Float
calculateEveningCompensation start end =
  let
    totalHours = getDeltaHours start end
    startingHour = hour start
    hours = range startingHour ((startingHour + totalHours) - 1)
      |> map ((flip (%)) 24)
    eveningHours = hours
      |> filter (\h -> h >= 18 || h < 6)
  in
    (toFloat (length eveningHours)) * 1.15

calculateOvertimeCompensation : Int -> Float
calculateOvertimeCompensation hours =
  let
    overtimeHours = hours - workdayLength
    getOvertime num memo =
         num < 2 => memo + hourlyWage * 0.25
      |= num < 4 => memo + hourlyWage * 0.5
      |= memo + hourlyWage
  in
    foldl getOvertime 0 <| range 0 <| overtimeHours - 1

getWage : HourMarking -> (Float, Float, Float)
getWage hours =
  let
    totalHours = getDeltaHours hours.start hours.end
    regular = (toFloat totalHours) * hourlyWage
    overtime = calculateOvertimeCompensation totalHours
    evening =
      calculateEveningCompensation hours.start hours.end
  in
    (regular, evening, overtime)


calculateByMarkings : List HourMarking -> Wage
calculateByMarkings hours =
  let
    personId = head hours
      |> Maybe.map .personId
      |> withDefault "_"
    addToWage hourMarking memo =
      let
        (regular, evening, overtime) = getWage hourMarking
      in
        { memo |
          regular = memo.regular + regular
        , evening = memo.evening + evening
        , overtime = memo.overtime + overtime
        }
  in
    foldl addToWage (Wage personId 0 0 0) hours


calculateWages : List HourMarking -> List Wage
calculateWages hours =
  let
    hoursByEmployee = groupWhile (\a b -> a.personId == b.personId) (sortBy .personId hours)
    wages = map calculateByMarkings hoursByEmployee
  in
    wages


