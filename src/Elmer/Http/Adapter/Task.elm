module Elmer.Http.Adapter.Task exposing
  ( HttpTaskRequestData
  , decode
  , toTaskResult
  )

import Elmer.Http.Adapter.Types exposing (..)
import Elmer.Http.Adapter as Adapter
import Elmer.Http.Types exposing (..)
import Elmer.Value as Value
import Http
import Json.Decode as Json


type alias HttpTaskRequestData err a =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , resolver : Http.Resolver err a
  , timeout : Maybe Float
  }


type TaskResultWrapper err a =
  TaskResultWrapper (Result err a)


decode : HttpTaskRequestData err a -> HttpRequestAdapter Http.Error (TaskResultWrapper err a)
decode taskData =
  toRequestData taskData
    |> Adapter.decode


toTaskResult : Exchange Http.Error (TaskResultWrapper err a) -> Result err a
toTaskResult exchange =
  case exchange.msg of
    TaskResultWrapper result ->
      result


toRequestData : HttpTaskRequestData err a -> HttpRequestData (TaskResultWrapper err a)
toRequestData taskData =
  { method = taskData.method
  , headers = taskData.headers
  , url = taskData.url
  , body = taskData.body
  , expect = Http.expectStringResponse TaskResultWrapper <| taskResolver taskData.resolver
  , timeout = taskData.timeout
  , tracker = Nothing
  }


taskResolver : Http.Resolver err a -> (Http.Response String -> Result err a)
taskResolver resolver =
  case Value.decode resolverDecoder resolver of
    Ok fun ->
      fun
    Err message ->
      Debug.todo <| "Problem parsing resolver: " ++ (Json.errorToString message)


resolverDecoder : Json.Decoder (Http.Response String -> Result err a)
resolverDecoder =
  Json.field "a" Value.decoder
