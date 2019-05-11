module Elmer.Http.Adapter exposing
  ( decode
  )

import Http
import Json.Decode as Json
import Elmer.Value as Value
import Elmer.Http.Routable as Routable
import Elmer.Http.Types exposing (..)
import Elmer.Http.Adapter.Types exposing (..)
import Elmer.Http.Adapter.Request as Request
import Elmer.Http.Adapter.ResponseHandler as ResponseHandler
import Elmer.Http.Response as Response
import Elmer.Message as Message exposing (fact, note)
import Elmer.Http.Errors as Errors



decode : HttpRequestData msg -> HttpRequestAdapter Http.Error msg
decode httpRequestData =
  { request = Request.decode httpRequestData
  , responseHandler = 
      ResponseHandler.decode httpRequestData
        |> handleResponse
  }


handleResponse : (Http.Response String -> msg) -> ResponseHandler Http.Error msg
handleResponse handler (stub, serverResult) =
  case serverResult of
    Response response ->
      handleResponseStatus response
        |> Ok << handler
    Error err ->
      case err of
        Http.Timeout ->
          Ok <| handler Http.Timeout_
        Http.NetworkError ->
          Ok <| handler Http.NetworkError_
        Http.BadUrl message ->
          Ok <| handler <| Http.BadUrl_ message
        Http.BadBody message ->
          Errors.errWith <|
            Errors.invalidUseOfBadBodyError (Routable.toString stub) message
        Http.BadStatus code ->
          Errors.errWith <|
            Errors.invalidUseOfBadStatusError (Routable.toString stub) code


handleResponseStatus : HttpResponse String -> Http.Response String
handleResponseStatus response =
  if Response.isGood response then
    Http.GoodStatus_ (Response.metadata response) response.body
  else
    Http.BadStatus_ (Response.metadata response) response.body