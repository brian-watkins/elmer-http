module Elmer.Http.Command exposing
  ( stubbedWith
  )

import Elmer.Http.Adapter as Adapter
import Elmer.Http.Adapter.Types exposing (..)
import Elmer.Http.Types exposing (..)
import Elmer.Http.Client as Client
import Elmer.Effects as Effects
import Elmer.Command as Command
import Http


type alias HttpRequestFunction msg =
  HttpRequestData msg -> Cmd msg


stubbedWith : List (HttpResponseStub Http.Error) -> HttpRequestFunction msg
stubbedWith responseStubs request =
  let
    adapter = Adapter.decode request
  in
    case Client.exchange responseStubs adapter of
      Ok exchange ->
        Command.fake exchange.msg
          |> deferIfNecessary exchange.stub
          |> toHttpCommand exchange.request
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
