module Components.WagesTable exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Utils.Wage exposing (Wage)

roundTo2Decimals : Float -> Float
roundTo2Decimals num =
  toFloat (round <| num * 100) / 100

euroFormat : Float -> String
euroFormat num =
  toString (roundTo2Decimals num) ++ " €"

value : String -> Html a
value val = div [ class "wages-table__value" ] [ text val ]

row : Wage -> Html a
row wage =
  tr [ class "wages-table__row" ]
    [ td [] [value wage.personId]
    , td [] [value <| euroFormat wage.regular]
    , td [] [value <| euroFormat wage.overtime]
    , td [] [value <| euroFormat wage.evening]
    , td []
      [ strong [] [value <| euroFormat (wage.regular + wage.evening + wage.overtime) ]]
    ]


wagesTable : List Wage -> Html a
wagesTable wages =
  div []
    [ table [ class "wages-table" ]
      [ thead []
        [ tr [ class "wages-table__header" ]
          [ th [] [ text "ID" ]
          , th [] [ text "Regular" ]
          , th [] [ text "Overtime" ]
          , th [] [ text "Evening" ]
          , th [] [ text "Total" ]
          ]
        ]
      , tbody [] (List.map row wages)
      ]
    ]
