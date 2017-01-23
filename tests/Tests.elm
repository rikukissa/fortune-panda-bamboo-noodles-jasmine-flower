module Tests exposing (..)

import Test exposing (..)
import List exposing (length, head)
import Maybe exposing (..)
import Expect

import Time.DateTime as DateTime exposing (DateTime, dateTime, zero)
import Utils.Wage exposing (Wage, calculateWages, HourMarking)

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
  describe "Sample Test Suite"
    [ describe "Wage calculator"
      [ test "returns as many wages as there are unique employees" <|
        \() ->
          let
            hours =
              [ (HourMarking "1" "Jorma Teräs"
                  (dateTime { zero | year = 2015, month = 10, day = 10, hour = 8 })
                  (dateTime { zero | year = 2015, month = 10, day = 10, hour = 16 }))
              , (HourMarking "1" "Jorma Teräs"
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 8 })
                  (dateTime { zero | year = 2015, month = 10, day = 11, hour = 16 }))
              , (HourMarking "2" "Foo Bar"
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
    ]
