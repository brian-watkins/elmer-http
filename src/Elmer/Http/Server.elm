module Elmer.Http.Server exposing
  ( HttpServerResult
  , handleRequest
  )

import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Types exposing (..)
import Elmer.Message exposing (..)


type alias HttpServerResult x a =
  { request : HttpRequest
  , stub: HttpStub x
  , result: Result x a
  }


handleRequest : List (HttpResponseStub x) -> HttpRequestHandler x a -> Result String (HttpServerResult x a)
handleRequest responseStubs requestHandler =
  unwrapResponseStubs responseStubs
    |> matchFirstRequest requestHandler
    |> Result.map (\stub ->
      { request = requestHandler.request
      , stub = stub
      , result = processResponse requestHandler stub
      }
    )


unwrapResponseStubs : List (HttpResponseStub x) -> List (HttpStub x)
unwrapResponseStubs responseStubs =
  List.map (\(HttpResponseStub stub) -> stub) responseStubs


matchFirstRequest : HttpRequestHandler x a -> List (HttpStub x) -> Result String (HttpStub x)
matchFirstRequest httpRequestHandler responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequestHandler) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| format
        [ fact "Received a request for" (printRequest httpRequestHandler)
        , fact "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequestHandler x a -> String
printRequest requestHandler =
  requestHandler.request.method ++ " " ++ requestHandler.request.url


printStubs : List (HttpStub x) -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : (HttpStub x) -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequestHandler x a -> (HttpStub x) -> Maybe (HttpStub x)
matchRequest httpRequestHandler stub =
  matchRequestUrl httpRequestHandler stub
    |> Maybe.andThen (matchRequestMethod httpRequestHandler)


matchRequestUrl : HttpRequestHandler x a -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestUrl httpRequestHandler stub =
  if (HttpInternal.route httpRequestHandler.request.url) == stub.url then
    Just stub
  else
    Nothing


matchRequestMethod : HttpRequestHandler x a -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestMethod httpRequestHandler stub =
  if httpRequestHandler.request.method == stub.method then
    Just stub
  else
    Nothing


processResponse : HttpRequestHandler x a -> (HttpStub x) -> Result x a
processResponse httpRequestHandler stub =
  buildResult stub httpRequestHandler.request
    |> Result.andThen httpRequestHandler.responseHandler


buildResult : HttpStub x -> HttpRequest -> Result x (HttpResponse String)
buildResult stub request =
  case stub.resultBuilder request of
    Response response ->
      Ok response
    Error error ->
      Err error
