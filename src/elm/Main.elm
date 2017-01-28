module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (on)
import Html.Attributes exposing (..)
import Json.Decode as JD
import List.Extra exposing (unique)

import Ports exposing (CSVData, fileSelected, fileContentRead)
import Utils.Wage exposing (Wage, calculateWages)
import Utils.HourMarking exposing (HourMarking, fromCSVRow)
import Components.WagesTable exposing (wagesTable)

type alias HourSheet = List HourMarking

type alias Model =
  { id : String
  , hourSheet : Maybe HourSheet
  , employees : List ( String, String )
  }

init : (Model, Cmd Msg)
init =
  ( { id = "fileInputId"
    , hourSheet = Nothing
    , employees = []
    }
  , Cmd.none
  )

type Msg
  = NoOp
  | FileSelected
  | CSVRowRead CSVData

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> ( model, Cmd.none )
    FileSelected ->
      ( { model | hourSheet = Nothing, employees = [] }
      , fileSelected model.id
      )
    CSVRowRead data ->
      ( { model |
          hourSheet =
            case (model.hourSheet, (Result.toMaybe (fromCSVRow data))) of
              (Nothing, Just hourMarking) -> Just [hourMarking]
              (Just hourSheet, Just hourMarking) -> Just (hourMarking :: hourSheet)
              (Just hourSheet, Nothing) -> Just hourSheet
              (Nothing, Nothing) -> Nothing
        , employees =
            case (Result.toMaybe (fromCSVRow data)) of
              Nothing -> model.employees
              Just hourSheet ->
                (hourSheet.personId, hourSheet.personName) :: model.employees
                  |> unique
        }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  let
    table = Maybe.map calculateWages model.hourSheet
      |> Maybe.map (wagesTable model.employees)
      |> Maybe.withDefault (div [] [])
  in
    div []
      [ h2 [] [ text "Upload hour markings" ]
      , input
          [ type_ "file"
          , id model.id
          , on "change"
              (JD.succeed FileSelected)] []
      , table
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
 fileContentRead CSVRowRead

main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
