module Elmer.Http.Errors exposing
  ( noRequest
  , wrongRequest
  , requestMatcherFailed
  , invalidUseOfBadBodyError
  , invalidUseOfBadStatusError
  , failWith
  , errWith
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


invalidUseOfBadBodyError : String -> String -> CustomError
invalidUseOfBadBodyError stubRequest message =
  [ note "Invalid use of Elmer.Http.Stub.withError!"
  , fact "It seems you wanted to stub" stubRequest
  , fact "to return a BadBody error with the message" message
  , note "To simulate a BadBody error, use Elmer.Http.Stub.withBody with a bad body instead."
  ]


invalidUseOfBadStatusError : String -> Int -> CustomError
invalidUseOfBadStatusError stubRequest code =
  [ note "Invalid use of Elmer.Http.Stub.withError!"
  , fact "It seems you wanted to stub" stubRequest
  , fact "to return a BadStatus error with the code" <| String.fromInt code
  , note "To simulate a BadStatus error, use Elmer.Http.Stub.withStatus with the bad status code instead."
  ]


failWith : CustomError -> Expectation
failWith =
  Expect.fail << format


errWith : CustomError -> Result String a
errWith =
  Err << format