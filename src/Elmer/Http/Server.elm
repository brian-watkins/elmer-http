module Elmer.Http.Server exposing
  ( HttpServerResult
  , handleRequest
  )

import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Types exposing (..)
import Elmer.Message exposing (..)


type alias HttpServerResult x msg =
  { request : HttpRequest
  , stub: HttpStub x
  , result: Result String msg
  }


handleRequest : List (HttpResponseStub x) -> HttpRequestHandler x msg -> Result String (HttpServerResult x msg)
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


matchFirstRequest : HttpRequestHandler x msg -> List (HttpStub x) -> Result String (HttpStub x)
matchFirstRequest httpRequestHandler responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequestHandler) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| format
        [ fact "Received a request for" (printRequest httpRequestHandler)
        , fact "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequestHandler x msg -> String
printRequest requestHandler =
  requestHandler.request.method ++ " " ++ requestHandler.request.url


printStubs : List (HttpStub x) -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : (HttpStub x) -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequestHandler x msg -> (HttpStub x) -> Maybe (HttpStub x)
matchRequest httpRequestHandler stub =
  matchRequestUrl httpRequestHandler stub
    |> Maybe.andThen (matchRequestMethod httpRequestHandler)


matchRequestUrl : HttpRequestHandler x msg -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestUrl httpRequestHandler stub =
  if (HttpInternal.route httpRequestHandler.request.url) == stub.url then
    Just stub
  else
    Nothing


matchRequestMethod : HttpRequestHandler x msg -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestMethod httpRequestHandler stub =
  if httpRequestHandler.request.method == stub.method then
    Just stub
  else
    Nothing


processResponse : HttpRequestHandler x msg -> (HttpStub x) -> Result String msg
processResponse httpRequestHandler stub =
  buildResult stub httpRequestHandler.request
    |> httpRequestHandler.responseHandler


buildResult : HttpStub x -> HttpRequest -> (HttpStub x, HttpResult x)
buildResult stub request =
  (stub, stub.resultBuilder request)
