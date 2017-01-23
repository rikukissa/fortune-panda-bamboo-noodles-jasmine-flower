module Utils.Wage exposing (Wage, HourMarking, calculateWages)

import Maybe exposing (withDefault)
import List exposing (head, filter, length, map, foldl, range)
import Guards exposing ((|=),(=>))
import List.Extra exposing (groupWhile)
import Time.DateTime as DateTime exposing (DateTime, dateTime, delta, hour)

workdayLength = 8
hourlyWage = 3.75

type alias Wage =
  { personId : String
  , regular : Float
  , evening : Float
  , overtime : Float
  }

type alias HourMarking =
  { personId : String
  , personName : String
  , start : DateTime
  , end : DateTime
  }

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
    hoursByEmployee = groupWhile (\a b -> a.personId == b.personId) hours
    wages = map calculateByMarkings hoursByEmployee
  in
    wages
