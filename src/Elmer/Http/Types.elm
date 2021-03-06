module Elmer.Http.Types exposing
    ( HttpEffects(..)
    , HttpHeader
    , HttpRequestAdapter
    , HttpRequest
    , HttpStub
    , HttpResponseStub(..)
    , HttpResult(..)
    , HttpStatus(..)
    , HttpRoute
    , HttpError(..)
    , HttpResponse
    , HttpStringBody
    , ResponseHandler
    , ExchangeResult
    , Exchange
    , Status
    )

import Dict exposing (Dict)


type HttpEffects
  = Requests

type alias HttpRoute =
  { method : String
  , url : String
  }

type alias HttpRequest =
  { method: String
  , url: String
  , headers: List HttpHeader
  , body: Maybe String
  }

type alias HttpHeader =
  { name: String
  , value: String
  }

type alias HttpStringBody =
  { mimeType: String
  , body: String
  }

type alias ResponseHandler x msg =
  (HttpStub x, HttpResult x) -> Result String msg

type alias HttpRequestAdapter x msg =
  { request: HttpRequest
  , responseHandler: ResponseHandler x msg
  }

type HttpResponseStub x
  = HttpResponseStub (HttpStub x)

type alias HttpStub x =
  { url: String
  , method: String
  , resultBuilder : (HttpRequest -> HttpResult x)
  , deferResponse: Bool
  }

type alias HttpResponse a =
  { url: String
  , status: Status
  , headers: Dict String String
  , body: a
  }

type HttpError
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus Int
  | UnparseableBody String

type alias Exchange x msg =
  { request : HttpRequest
  , stub : HttpStub x
  , msg : msg
  }

type alias ExchangeResult x msg =
  Result String (Exchange x msg)


type HttpResult x
  = Response (HttpResponse String)
  | Error x

type HttpStatus
  = HttpStatus Status

type alias Status =
  { code: Int
  , message: String
  }
