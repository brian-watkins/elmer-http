module MethodTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Command as Command
import Elmer.Spy as Spy
import Elmer.Http exposing (HttpResponseStub)
import Elmer.Http.Stub as Stub
import Elmer.Http.Route exposing (HttpRoute, get, post)
import Elmer.Http.Matchers exposing (wasRequested, hasBody)
import Http


getTests : Test
getTests =
  describe "when Http.get is used to make a request" <|
  let
      testState =
        Command.given (\_ -> 
          Http.get
            { url = "http://fun.com/fun.html"
            , expect = Http.expectString StringResult 
            }
        )
        |> Spy.use 
          [ Elmer.Http.serve [ funStub <| get "http://fun.com/fun.html" ] 
          ]
  in
    [ test "it returns the stubbed response" <|
      \() ->
        testState
          |> Command.expectMessages (
            exactly 1 <| Expect.equal <|
              StringResult <| Ok "{\"name\":\"Cool Dude\"}"
          )
    , test "it records the request" <|
      \() ->
        testState
          |> Elmer.Http.expect (get "http://fun.com/fun.html") (
            wasRequested 1
          )
    ]


postTests : Test
postTests =
  describe "when Http.post is used to make a request" <|
  let
    testState =
      Command.given (\_ -> 
        Http.post
          { url = "http://fun.com/fun.html"
          , body = Http.stringBody "text/plan" "Yo yo yo"
          , expect = Http.expectString StringResult 
          }
      )
      |> Spy.use
        [ Elmer.Http.serve [ funStub <| post "http://fun.com/fun.html" ]
        ]
  in
    [ test "it returns the stubbed response" <|
      \() ->
        testState
          |> Command.expectMessages (
            exactly 1 <| Expect.equal <|
              StringResult <| Ok "{\"name\":\"Cool Dude\"}"
          )
    , test "it records the request" <|
      \() ->
        testState
          |> Elmer.Http.expect (post "http://fun.com/fun.html") (
            wasRequested 1
          )
    , test "it records the request body" <|
      \() ->
        testState
          |> Elmer.Http.expect (post "http://fun.com/fun.html") (
            exactly 1 <| hasBody "Yo yo yo"
          )
    ]


type TestMsg
  = StringResult (Result Http.Error String)


funStub : HttpRoute -> HttpResponseStub
funStub route =
  Stub.for route
    |> Stub.withBody "{\"name\":\"Cool Dude\"}"