module Elmer.Http.Server exposing
  ( HttpStubMatch
  , matchStub
  )

import Elmer.Http.Routable as Routable
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
  case List.head <| List.filter (Routable.matches httpRequest) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| format
        [ fact "Received a request for" (printRequest httpRequest)
        , fact "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequest -> String
printRequest =
  Routable.toString


printStubs : List (HttpStub x) -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : (HttpStub x) -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url
