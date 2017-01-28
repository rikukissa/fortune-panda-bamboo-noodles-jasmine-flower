module Tests exposing (..)

import Test exposing (..)
import List exposing (length, head)
import Maybe exposing (..)
import Expect

import Time.DateTime as DateTime exposing (DateTime, dateTime, zero)
import Utils.Wage exposing (Wage, calculateWages)
import Utils.HourMarking exposing (HourMarking, fromCSVRow)

getWageForSingleMarking : DateTime -> DateTime -> Wage
getWageForSingleMarking start end =
  let
    hours =
      [ (HourMarking "1" "Jorma Teräs" start end)
      ]
    wages = calculateWages hours
  in
    head wages |> withDefault (Wage "1" 0 0 0)

all : Test
all =
  describe "Wage calculator"
    [ describe "General functionality"
      [ test "returns as many wages as there are unique employees" <|
        \() ->
          let
            hours =
              [ (HourMarking "1" "Jorma Teräs"
                  (dateTime { zero | year = 2015, month = 10, day = 10, hour = 8 })
                  (dateTime { zero | year = 2015, month = 10, day = 10, hour = 16 }))
              , (HourMarking "2" "Foo Bar"
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 8 })
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 16 }))
              , (HourMarking "1" "Jorma Teräs"
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 8 })
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 16 }))
              ]
          in
            Expect.equal (length (calculateWages hours)) 2

      , test "uses $3.75 as the regular wage for employees" <|
        \() ->
          let
            wage = getWageForSingleMarking
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 8 })
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 16 })
          in
            Expect.equal wage.regular (8 * 3.75)

      , test "calculates overtime to total wage" <|
        \() ->
          let
            wage = getWageForSingleMarking
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 8 })
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 23 })
          in
            Expect.all
              [ \(wage) -> Expect.equal wage.regular (15 * 3.75)
              , \(wage) -> Expect.equal wage.overtime <| (2 * 3.75 * 0.25) + (2 * 3.75 * 0.5) + (3.75 * 3)
              ]
              wage

      ]

    , describe "Evening compensation calculation returns correct wage for"
      [ test "regular hours" <|
        \() ->
          let
            wage = getWageForSingleMarking
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 15 })
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 23 })
          in
            Expect.equal wage.evening (5 * 1.15)

      , test "shifts extending to 2 days" <|
        \() ->
          let
            wage = getWageForSingleMarking
              (dateTime { zero | year = 2015, month = 10, day = 10, hour = 19 })
              (dateTime { zero | year = 2015, month = 10, day = 11, hour = 03 })
          in
            Expect.equal wage.evening (8 * 1.15)

      , test "shift that happen during nighttime" <|
        \() ->
          let
            wage = getWageForSingleMarking
              (dateTime { zero | year = 2015, month = 10, day = 11, hour = 01 })
              (dateTime { zero | year = 2015, month = 10, day = 11, hour = 07 })
          in
            Expect.equal wage.evening (5 * 1.15)
      ]
    , describe "CSV row parser"
      [ test "parses strings to HourMarking instances" <|
        \() ->
          case (fromCSVRow "nimi, 8, 26.3.2014, 13:15, 21:00") of
            Ok marking ->
              Expect.equal marking
                (HourMarking "8" "nimi"
                  (dateTime { zero | year = 2014, month = 3, day = 26, hour = 13, minute = 15})
                  (dateTime { zero | year = 2014, month = 3, day = 26, hour = 21 }))

            Err _ ->
              Expect.fail "Failed to parse csv row"
      , test "interprets end date as the next day when hour is >24" <|
        \() ->
          case (fromCSVRow "nimi, 8, 26.3.2014, 13:15, 02:00") of
            Ok marking ->
              Expect.equal marking
                (HourMarking "8" "nimi"
                  (dateTime { zero | year = 2014, month = 3, day = 26, hour = 13, minute = 15})
                  (dateTime { zero | year = 2014, month = 3, day = 27, hour = 02 }))

            Err _ ->
              Expect.fail "Failed to parse csv row"
      ]
    ]
