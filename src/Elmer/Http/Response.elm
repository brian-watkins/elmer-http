module Elmer.Http.Response exposing
  ( isGood
  , metadata
  )

import Elmer.Http.Types exposing (..)
import Http


isGood : HttpResponse a -> Bool
isGood response =
  response.status.code >= 200 && response.status.code < 300


metadata : HttpResponse a -> Http.Metadata
metadata response =
  { url = response.url
  , statusCode = response.status.code
  , statusText = response.status.message
  , headers = response.headers
  }
