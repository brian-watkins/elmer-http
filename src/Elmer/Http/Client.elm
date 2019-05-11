module Elmer.Http.Client exposing
  ( exchange
  )

import Elmer.Http.Server as Server exposing (HttpStubMatch)
import Elmer.Http.Types exposing (..)
import Http


exchange : List (HttpResponseStub x) -> HttpRequestAdapter x msg -> ExchangeResult x msg
exchange stubs adapter =
  Server.matchStub stubs adapter.request
    |> Result.map buildResult
    |> Result.andThen (handleExchange adapter)


buildResult : HttpStubMatch x -> (HttpStub x, HttpResult x)
buildResult serverResponse =
  ( serverResponse.stub
  , serverResponse.request
    |> serverResponse.stub.resultBuilder
  )


handleExchange : HttpRequestAdapter x msg -> (HttpStub x, HttpResult x) -> ExchangeResult x msg
handleExchange adapter (stub, result) =
  adapter.responseHandler (stub, result)
    |> Result.map (\msg ->
      { request = adapter.request
      , stub = stub
      , msg = msg
      }
    )