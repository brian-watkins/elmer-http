module Elmer.Http.Task exposing
  ( stubbedWith
  )

import Elmer.Http.Adapter as Adapter
import Elmer.Http.Types exposing (..)
import Elmer.Http.Client as Client
import Elmer.Task
import Elmer.Effects as Effects
import Task exposing (Task)
import Http


stubbedWith : List (HttpResponseStub Http.Error) -> Adapter.HttpTaskRequestData err a -> Task err a
stubbedWith responseStubs taskRequest =
  let
      adapter =
        Adapter.toRequestData taskRequest
          |> Adapter.asHttpRequestAdapter
  in
    case Client.exchange responseStubs adapter of
      Ok exchange ->
        case Adapter.toTaskResult exchange of
          Ok val ->
            Task.succeed val
              |> toHttpTask exchange
          Err err ->
            Task.fail err
              |> toHttpTask exchange
      Err error ->
        Elmer.Task.failTest error


toHttpTask : Exchange x msg -> Task err a -> Task err a
toHttpTask exchange task =
  task
    |> deferIfNecessary exchange.stub
    |> andRecordRequestTask exchange.request

andRecordRequestTask : HttpRequest -> Task err a -> Task err a
andRecordRequestTask request =
  updateTestState request
    |> Effects.pushWithTask Requests


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


deferIfNecessary : HttpStub x -> Task err a -> Task err a
deferIfNecessary stub task =
  if stub.deferResponse then
    Elmer.Task.defer task
  else
    task
