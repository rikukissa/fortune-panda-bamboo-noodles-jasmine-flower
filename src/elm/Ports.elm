port module Ports exposing (..)

type alias CSVData = String

port fileSelected : String -> Cmd msg

port fileContentRead : (CSVData -> msg) -> Sub msg
