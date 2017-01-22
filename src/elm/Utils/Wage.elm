module Utils.Wage exposing (Wage, HourMarking, calculateWages)

import List exposing (filter, length, map, foldl, range)
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

getWage : HourMarking -> Wage
getWage hours =
  let
    totalHours = getDeltaHours hours.start hours.end
    regular = (toFloat totalHours) * hourlyWage
    overtime = calculateOvertimeCompensation totalHours
    evening =
      if overtime > 0 then
        0
      else
        calculateEveningCompensation hours.start hours.end
  in
    (Wage "1" regular evening overtime)


sumMarkings : List HourMarking -> Wage
sumMarkings hours =
  let
    add hourMarking memo =
      let
        wage = getWage hourMarking
      in
        (Wage "1" (memo.regular + wage.regular) (memo.evening + wage.evening) (memo.overtime + wage.overtime))
  in
    foldl add (Wage "1" 0 0 0) hours


calculateWages : List HourMarking -> List Wage
calculateWages hours =
  let
    hoursByEmployee = groupWhile (\a b -> a.personId == b.personId) hours
    wages = map sumMarkings hoursByEmployee
  in
    wages
