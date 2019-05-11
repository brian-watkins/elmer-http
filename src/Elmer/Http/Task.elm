module Elmer.Http.Task exposing
  ( stubbedWith
  )

import Elmer.Http.Adapter as Adapter
import Elmer.Http.Types exposing (..)
import Elmer.Http.Server as Server
import Elmer.Task
import Elmer.Effects as Effects
import Task exposing (Task)
import Http


stubbedWith : List (HttpResponseStub Http.Error) -> Adapter.HttpTaskRequestData x a -> Task x a
stubbedWith responseStubs taskRequest =
  let
      requestHandler = 
        Adapter.toRequestData taskRequest
          |> Adapter.asHttpRequestHandler
  in
    case Server.handleRequest responseStubs requestHandler of
      Ok response ->
        case response.result of
          Ok (Adapter.Wrapper result) ->
            case result of
              Ok val ->
                Task.succeed val
                  |> deferIfNecessary response.stub
                  |> andRecordRequestTask requestHandler.request
              Err err ->
                Task.fail err
                  |> deferIfNecessary response.stub
                  |> andRecordRequestTask requestHandler.request
          Err error ->
            Elmer.Task.failTest error
      Err error ->
        Elmer.Task.failTest error


andRecordRequestTask : HttpRequest -> Task x a -> Task x a
andRecordRequestTask request =
  updateTestState request
    |> Effects.pushWithTask Requests


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


deferIfNecessary : HttpStub Http.Error -> Task x a -> Task x a
deferIfNecessary stub task =
  if stub.deferResponse then
    Elmer.Task.defer task
  else
    task
