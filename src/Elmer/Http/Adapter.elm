module Elmer.Http.Adapter exposing
  ( HttpRequestData
  , HttpTaskRequestData
  , asHttpRequestHandler
  , makeHttpRequest
  , toRequestData
  , Wrapper(..)
  )

import Http
import Json.Decode as Json
import Elmer.Value as Value
import Elmer.Http.Internal exposing (isGoodStatusCode, routeToString)
import Elmer.Http.Types exposing (..)
import Elmer.Message as Message exposing (fact, note)
import Elmer.Http.Errors as Errors


type alias HttpRequestData msg =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , expect : Http.Expect msg
  , timeout : Maybe Float
  , tracker : Maybe String
  }

type alias HttpTaskRequestData x a =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , resolver : Http.Resolver x a
  , timeout : Maybe Float
  }

type Wrapper x a =
  Wrapper (Result x a)

toRequestData : HttpTaskRequestData x a -> HttpRequestData (Wrapper x a)
toRequestData taskData =
  { method = taskData.method
  , headers = taskData.headers
  , url = taskData.url
  , body = taskData.body
  , expect = Http.expectStringResponse Wrapper <| taskResolver taskData.resolver
  , timeout = taskData.timeout
  , tracker = Nothing
  }

taskResolver : Http.Resolver x a -> (Http.Response String -> Result x a)
taskResolver resolver =
  case Value.decode resolverDecoder resolver of
    Ok fun ->
      fun
    Err message ->
      Debug.todo <| "Problem parsing resolver: " ++ (Json.errorToString message)

resolverDecoder : Json.Decoder (Http.Response String -> Result x a)
resolverDecoder =
  Json.field "a" Value.decoder

asHttpRequestHandler : HttpRequestData msg -> HttpRequestHandler Http.Error msg
asHttpRequestHandler httpRequest =
  { request = makeHttpRequest httpRequest
  , responseHandler =
      case Value.decode expectDecoder httpRequest of
        Ok handler ->
          \(stub, serverResult) ->
            case serverResult of
              Response response ->
                handleResponseStatus response
                  |> handler
                  |> Ok
              Error err ->
                case err of
                  Http.Timeout ->
                    Ok <| handler Http.Timeout_
                  Http.NetworkError ->
                    Ok <| handler Http.NetworkError_
                  Http.BadUrl message ->
                    Ok <| handler <| Http.BadUrl_ message
                  Http.BadBody message ->
                    Errors.errWith <|
                      Errors.invalidUseOfBadBodyError (routeToString stub) message
                  Http.BadStatus code ->
                    Errors.errWith <|
                      Errors.invalidUseOfBadStatusError (routeToString stub) code
        Err err ->
          "Error fetching " ++ Json.errorToString err
            |> Debug.todo
  }



handleResponseStatus : HttpResponse String -> Http.Response String
handleResponseStatus response =
  if isGoodStatusCode response.status.code then
    Http.GoodStatus_ (toMetadata response) response.body
  else
    Http.BadStatus_ (toMetadata response) response.body


toMetadata : HttpResponse String -> Http.Metadata
toMetadata response =
  { url = response.url
  , statusCode = response.status.code
  , statusText = response.status.message
  , headers = response.headers
  }

expectDecoder : Json.Decoder (Http.Response String -> msg)
expectDecoder =
  Json.at ["expect", "a"] Value.decoder


makeHttpRequest : HttpRequestData msg -> HttpRequest
makeHttpRequest request =
  case Value.decode httpRequestDecoder request of
    Ok r -> 
      r
    Err msg ->
      "Unable to decode Http.Request: " ++ Json.errorToString msg
        |> Debug.todo


httpRequestDecoder : Json.Decoder HttpRequest
httpRequestDecoder =
  Json.map4 HttpRequest
    (Json.field "method" Json.string)
    (Json.field "url" Json.string)
    (Json.field "headers" (Value.list headerDecoder))
    (Json.field "body" bodyDecoder)


headerDecoder : Json.Decoder HttpHeader
headerDecoder =
  Json.map2 HttpHeader
    (Value.firstArg Json.string)
    (Value.secondArg Json.string)


bodyDecoder : Json.Decoder (Maybe String)
bodyDecoder =
  Json.oneOf
  [ mimeTypeBodyDecoder
  , emptyBodyDecoder
  ]

mimeTypeBodyDecoder : Json.Decoder (Maybe String)
mimeTypeBodyDecoder =
  Json.map2 HttpStringBody
    (Value.firstArg Json.string)
    (Value.secondArg Json.string)
    |> Json.map .body
    |> Json.map Just

emptyBodyDecoder : Json.Decoder (Maybe String)
emptyBodyDecoder =
  Json.succeed Nothing
  
