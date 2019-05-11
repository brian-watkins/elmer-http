module Elmer.Http.Server exposing
  ( HttpStubMatch
  , matchStub
  )

import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Types exposing (..)
import Elmer.Message exposing (..)


type alias HttpStubMatch x =
  { request : HttpRequest
  , stub: HttpStub x
  }


matchStub : List (HttpResponseStub x) -> HttpRequest -> Result String (HttpStubMatch x)
matchStub responseStubs request =
  unwrapResponseStubs responseStubs
    |> matchFirstRequest request
    |> Result.map (\stub ->
      { request = request
      , stub = stub
      }
    )


unwrapResponseStubs : List (HttpResponseStub x) -> List (HttpStub x)
unwrapResponseStubs responseStubs =
  List.map (\(HttpResponseStub stub) -> stub) responseStubs


matchFirstRequest : HttpRequest -> List (HttpStub x) -> Result String (HttpStub x)
matchFirstRequest httpRequest responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequest) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| format
        [ fact "Received a request for" (printRequest httpRequest)
        , fact "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequest -> String
printRequest =
  HttpInternal.routeToString


printStubs : List (HttpStub x) -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : (HttpStub x) -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequest -> (HttpStub x) -> Maybe (HttpStub x)
matchRequest request stub =
  matchRequestUrl request stub
    |> Maybe.andThen (matchRequestMethod request)


matchRequestUrl : HttpRequest -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestUrl request stub =
  if (HttpInternal.route request.url) == stub.url then
    Just stub
  else
    Nothing


matchRequestMethod : HttpRequest -> (HttpStub x) -> Maybe (HttpStub x)
matchRequestMethod request stub =
  if request.method == stub.method then
    Just stub
  else
    Nothing
