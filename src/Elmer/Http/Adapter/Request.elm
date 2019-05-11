module Elmer.Http.Adapter.Request exposing
  ( decode
  )

import Elmer.Http.Adapter.Types exposing (..)
import Elmer.Http.Types exposing (..)
import Elmer.Value as Value
import Json.Decode as Json
import Http



decode : HttpRequestData msg -> HttpRequest
decode request =
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
