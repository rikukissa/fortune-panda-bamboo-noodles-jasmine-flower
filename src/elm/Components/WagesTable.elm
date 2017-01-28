module Components.WagesTable exposing (..)

import Html exposing (..)
import Tuple exposing (first, second)
import Html.Attributes exposing (class, src)

import Utils.Wage exposing (Wage)
import List.Extra exposing (find)

roundTo2Decimals : Float -> Float
roundTo2Decimals num =
  toFloat (round <| num * 100) / 100

euroFormat : Float -> String
euroFormat num =
  toString (roundTo2Decimals num) ++ " €"

value : Html a -> Html a
value val = div [ class "wages-table__value" ] [ val ]

row : List (String, String) -> Wage -> Html a
row employees wage =
  let
    personName = find (first >> ((==) wage.personId)) employees
      |> Maybe.map second
      |> Maybe.withDefault "?"
    totalWage = wage.regular + wage.evening + wage.overtime
  in
    tr [ class "wages-table__row" ]
      [ td []
        [ value <| div []
          [ img [ src <| "http://i.pravatar.cc/20?" ++ wage.personId ] []
          , text personName
          ]
        ]
      , td [] [ value <| text <| euroFormat wage.regular ]
      , td [] [ value <| text <| euroFormat wage.overtime ]
      , td [] [ value <| text <| euroFormat wage.evening ]
      , td []
        [ strong [] [value <| text <| euroFormat totalWage ]]
      ]


wagesTable : List (String, String) -> List Wage -> Html a
wagesTable employees wages =
  div []
    [ table [ class "wages-table" ]
      [ thead []
        [ tr [ class "wages-table__header" ]
          [ th [] [ text "Name" ]
          , th [] [ text "Regular" ]
          , th [] [ text "Overtime" ]
          , th [] [ text "Evening" ]
          , th [] [ text "Total" ]
          ]
        ]
      , tbody [] (List.map (row employees) wages)
      ]
    ]
