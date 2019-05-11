module Elmer.Http.Adapter.Types exposing
  ( HttpRequestData
  )

import Http


type alias HttpRequestData msg =
  { method : String
  , headers : List Http.Header
  , url : String
  , body : Http.Body
  , expect : Http.Expect msg
  , timeout : Maybe Float
  , tracker : Maybe String
  }
