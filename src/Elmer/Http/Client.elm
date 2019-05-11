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
    |> Result.andThen (\(stub, result) ->
      adapter.responseHandler (stub, result)
        |> Result.map (\msg ->
          { request = adapter.request
          , stub = stub
          , msg = msg
          }
        )
    )


processStubbedResponse : ResponseHandler x msg -> HttpStubMatch x -> Result String msg
processStubbedResponse handler serverResponse =
  buildResult serverResponse
    |> handler


buildResult : HttpStubMatch x -> (HttpStub x, HttpResult x)
buildResult serverResponse =
  ( serverResponse.stub
  , serverResponse.request
    |> serverResponse.stub.resultBuilder
  )
