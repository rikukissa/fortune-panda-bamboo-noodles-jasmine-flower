module Main exposing (..)
import Html exposing (..)
import Html.Events exposing (on)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Ports exposing (CSVData, fileSelected, fileContentRead)
import Utils.Wage exposing (HourMarking, Wage, fromCSVRow, calculateWages)

type alias HourSheet = List HourMarking

type alias Model =
  { id : String
  , hourSheet : Maybe HourSheet
  }

init : ( Model, Cmd Msg )
init =
  ( { id = "fileInputId"
    , hourSheet = Nothing
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
      ( { model | hourSheet = Nothing }
      , fileSelected model.id
      )
    CSVRowRead data ->
      ( { model | hourSheet =
            case (model.hourSheet, (Result.toMaybe (fromCSVRow data))) of
              (Nothing, Just hourMarking) -> Just [hourMarking]
              (Just hourSheet, Just hourMarking) -> Just (hourMarking :: hourSheet)
              (Just hourSheet, Nothing) -> Just hourSheet
              (Nothing, Nothing) -> Nothing

        }
      , Cmd.none
      )

row : Wage -> Html Msg
row wage =
  tr []
    [ td [] [text wage.personId]
    , td [] [text <| toString wage.regular]
    , td [] [text <| toString wage.overtime]
    , td [] [text <| toString wage.evening]
    , td [] [text <| toString (wage.regular + wage.evening + wage.overtime)]
    ]

view : Model -> Html Msg
view model =
  let
    wages = Maybe.map calculateWages model.hourSheet
      |> Maybe.withDefault []
  in

    div []
      [ text ((toString << (Maybe.withDefault 0) << (Maybe.map List.length)) model.hourSheet)
      , input
          [ type_ "file"
          , id model.id
          , on "change"
              (JD.succeed FileSelected)] []
      , table []
        [ thead []
          [ tr []
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
