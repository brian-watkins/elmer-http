module Elmer.Http.Errors exposing
  ( noRequest
  , wrongRequest
  , requestMatcherFailed
  , failWith
  )

import Expect exposing (Expectation)
import Elmer.Message exposing (..)

type alias CustomError
  = List Message


noRequest : String -> CustomError
noRequest expectedRoute =
  [ fact "Expected request for" expectedRoute
  , note "but no requests have been made"
  ]


wrongRequest : String -> String -> CustomError
wrongRequest expectedRoute actualRequests =
  [ fact "Expected request for" expectedRoute
  , fact "but only found these requests" actualRequests
  ]

requestMatcherFailed : String -> String -> CustomError
requestMatcherFailed expectedRoute failure =
  [ fact "Requests matching" expectedRoute
  , note "failed to meet the expectations:"
  , note failure
  ]


failWith : CustomError -> Expectation
failWith =
  Expect.fail << format