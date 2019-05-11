module Elmer.Http.Response exposing
  ( isGood
  )

import Elmer.Http.Types exposing (..)


isGood : HttpResponse a -> Bool
isGood response =
  response.status.code >= 200 && response.status.code < 300
