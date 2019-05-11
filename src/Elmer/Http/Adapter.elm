module Elmer.Http.Adapter exposing
  ( HttpRequestData
  , HttpTaskRequestData
  , asHttpRequestAdapter
  , makeHttpRequest
  , toRequestData
  , toTaskResult
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


type alias HttpTaskRequestData err a =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , resolver : Http.Resolver err a
  , timeout : Maybe Float
  }


type TaskResultWrapper err a =
  TaskResultWrapper (Result err a)


toRequestData : HttpTaskRequestData err a -> HttpRequestData (TaskResultWrapper err a)
toRequestData taskData =
  { method = taskData.method
  , headers = taskData.headers
  , url = taskData.url
  , body = taskData.body
  , expect = Http.expectStringResponse TaskResultWrapper <| taskResolver taskData.resolver
  , timeout = taskData.timeout
  , tracker = Nothing
  }


taskResolver : Http.Resolver err a -> (Http.Response String -> Result err a)
taskResolver resolver =
  case Value.decode resolverDecoder resolver of
    Ok fun ->
      fun
    Err message ->
      Debug.todo <| "Problem parsing resolver: " ++ (Json.errorToString message)


resolverDecoder : Json.Decoder (Http.Response String -> Result err a)
resolverDecoder =
  Json.field "a" Value.decoder


toTaskResult : Exchange Http.Error (TaskResultWrapper err a) -> Result err a
toTaskResult exchange =
  case exchange.msg of
    TaskResultWrapper result ->
      result


asHttpRequestAdapter : HttpRequestData msg -> HttpRequestAdapter Http.Error msg
asHttpRequestAdapter httpRequestData =
  { request = makeHttpRequest httpRequestData
  , responseHandler = makeHandler httpRequestData
  }


makeHandler : HttpRequestData msg -> ResponseHandler Http.Error msg
makeHandler httpRequestData =
  case Value.decode expectDecoder httpRequestData of
    Ok handler ->
      responseHandler handler
    Err err ->
      "Error parsing expect function " ++ Json.errorToString err
        |> Debug.todo


responseHandler : (Http.Response String -> msg) -> ResponseHandler Http.Error msg
responseHandler handler (stub, serverResult) =
  case serverResult of
    Response response ->
      handleResponseStatus response
        |> Ok << handler
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
  
