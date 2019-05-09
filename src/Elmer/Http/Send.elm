module Elmer.Http.Send exposing
  ( stubbedWith
  )

import Elmer.Http.Adapter as Adapter
import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Types exposing (..)
import Elmer.Http.Server as Server
import Elmer.Effects as Effects
import Elmer.Command as Command
import Elmer.Message exposing (..)
import Http


type alias HttpRequestFunction a msg =
  (Result Http.Error a -> msg) -> Http.Request a -> Cmd msg


stubbedWith : List (HttpResponseStub Http.Error) -> HttpRequestFunction a msg
stubbedWith responseStubs tagger request =
  let
    requestHandler = Adapter.asHttpRequestHandler request
  in
  case Server.handleRequest responseStubs requestHandler of
    Ok response ->
      case response.result of
        Ok data ->
          generateCommand response.stub tagger data
            |> toHttpCommand response.request
        Err error ->
          failCommand response.request tagger error
            |> toHttpCommand response.request
    Err error ->
      Command.fail error


failCommand : HttpRequest -> (Result Http.Error a -> msg) -> Http.Error -> Cmd msg
failCommand httpRequest tagger error =
  case error of
    Http.BadPayload msg response ->
      Command.fail <| format
        [ fact "Parsing a stubbed response" (httpRequest.method ++ " " ++ httpRequest.url)
        , note <| "\tWith body: " ++ (printBody response.body)
        , fact "failed with error" msg
        , note "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
        ]
    _ ->
      Command.fake (tagger (Err error))


generateCommand : HttpStub Http.Error -> (Result Http.Error a -> msg) -> a -> Cmd msg
generateCommand stub tagger data =
  let
    command = Command.fake (tagger (Ok data))
  in
    if stub.deferResponse then
      Command.defer command
    else
      command


toHttpCommand : HttpRequest -> Cmd msg -> Cmd msg
toHttpCommand request command =
  let
    httpCommand = Effects.push Requests <| updateTestState request
  in
    Cmd.batch [ httpCommand, command ]


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


printBody : String -> String
printBody body =
  "\"" ++ body ++ "\""
