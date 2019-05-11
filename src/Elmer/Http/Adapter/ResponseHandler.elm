module Elmer.Http.Adapter.ResponseHandler exposing 
  ( decode
  )

import Elmer.Http.Adapter.Types exposing (..)
import Elmer.Value as Value
import Json.Decode as Json
import Http


decode : HttpRequestData msg -> (Http.Response String -> msg)
decode httpRequestData =
  case Value.decode expectDecoder httpRequestData of
    Ok handler ->
      handler
    Err err ->
      "Error parsing expect function " ++ Json.errorToString err
        |> Debug.todo


expectDecoder : Json.Decoder (Http.Response String -> msg)
expectDecoder =
  Json.at ["expect", "a"] Value.decoder
