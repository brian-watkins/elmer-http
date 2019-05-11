module Elmer.Http.Command exposing
  ( stubbedWith
  )

import Elmer.Http.Adapter as Adapter
import Elmer.Http.Types exposing (..)
import Elmer.Http.Server as Server
import Elmer.Effects as Effects
import Elmer.Command as Command
import Http


type alias HttpRequestFunction msg =
  Adapter.HttpRequestData msg -> Cmd msg


stubbedWith : List (HttpResponseStub Http.Error) -> HttpRequestFunction msg
stubbedWith responseStubs request =
  let
    requestHandler = Adapter.asHttpRequestHandler request
  in
    case Server.handleRequest responseStubs requestHandler of
      Ok response ->
        case response.result of
          Ok msg ->
            Command.fake msg
              |> deferIfNecessary response.stub
              |> toHttpCommand response.request
          Err error ->
            Command.fail error
      Err error ->
        Command.fail error


deferIfNecessary : HttpStub Http.Error -> Cmd msg -> Cmd msg
deferIfNecessary stub command =
  if stub.deferResponse then
    Command.defer command
  else
    command


toHttpCommand : HttpRequest -> Cmd msg -> Cmd msg
toHttpCommand request command =
  Cmd.batch 
    [ Effects.push Requests <| updateTestState request
    , command
    ]


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request
