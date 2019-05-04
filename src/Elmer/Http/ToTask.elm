module Elmer.Http.ToTask exposing
  ( stubbedWith
  )


import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Types exposing (..)
import Elmer.Http.Server as Server
import Elmer.Task
import Elmer.Effects as Effects
import Elmer.Command as Command
import Task exposing (Task)
import Http


stubbedWith : List HttpResponseStub -> Http.Request a -> Task Http.Error a
stubbedWith responseStubs request =
  case Server.handleRequest responseStubs request of
    Ok response ->
      httpTask response.request response.result
        |> deferIfNecessary response.stub
    Err error ->
      Elmer.Task.failTest error


httpTask : HttpRequest -> Result Http.Error a -> Task Http.Error a
httpTask request result =
  case result of
    Ok value ->
      Task.succeed value
        |> andRecordRequestTask request
    Err error ->
      Task.fail error
        |> andRecordRequestTask request


andRecordRequestTask : HttpRequest -> Task x a -> Task x a
andRecordRequestTask request =
  updateTestState request
    |> Effects.pushWithTask Requests


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


deferIfNecessary : HttpStub -> Task Http.Error a -> Task Http.Error a
deferIfNecessary stub task =
  if stub.deferResponse then
    Elmer.Task.defer task
  else
    task
