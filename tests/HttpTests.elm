module HttpTests exposing (..)

import Test exposing (..)
import Expect
import Http
import Dict
import Elmer
import Html exposing (Html)
import Json.Decode as Json
import Elmer exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Http as ElmerHttp exposing (HttpResponseStub)
import Elmer.Http.Stub as HttpStub
import Elmer.Http.Status as Status
import Elmer.Http.Matchers exposing (..)
import Elmer.Http.Route as Route exposing (HttpRoute)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Spy as Spy
import Elmer.Message exposing (..)
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Http.Errors as Errors

import TestApps.HttpTestApp as App
import TestApps.SimpleTestApp as SimpleApp


serveTests : Test
serveTests =
  describe "serve"
  [ describe "when the requested url is not stubbed"
    [ test "it fails with a message" <|
      \() ->
        let
          stubbedResponse = HttpStub.for (Route.get "http://wrongUrl.com")
            |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
          anotherStubbedResponse = HttpStub.for (Route.post "http://whatUrl.com")
            |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
        in
          Elmer.given App.defaultModel App.view App.update
            |> Spy.use [ ElmerHttp.serve [ stubbedResponse, anotherStubbedResponse ] ]
            |> Markup.target << by [ id "request-data-click" ]
            |> Event.click
            |> Markup.target << by [ id "data-result" ]
            |> Markup.expect (element <| hasText "Name: Super Fun Person")
            |> Expect.equal (Expect.fail (format
              [ fact "Received a request for" "GET http://fun.com/fun.html"
              , fact "but it does not match any of the stubbed requests" "GET http://wrongUrl.com\nPOST http://whatUrl.com"
              ]
            ))
    ]
  , describe "when the requested url matches the stubbed response"
    [ describe "when the method does not match"
      [ test "it fails with a message" <|
        \() ->
          let
            stubbedResponse = HttpStub.for (Route.post "http://fun.com/fun.html")
              |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
          in
            Elmer.given App.defaultModel App.view App.update
              |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
              |> Markup.target << by [ id "request-data-click" ]
              |> Event.click
              |> Markup.target << by [ id "data-result" ]
              |> Markup.expect (element <| hasText "Name: Super Fun Person")
              |> Expect.equal (Expect.fail (format
                [ fact "Received a request for" "GET http://fun.com/fun.html"
                , fact "but it does not match any of the stubbed requests" "POST http://fun.com/fun.html"
                ]
              ))
      ]
    , describe "when the method matches"
      [ describe "when the response status is outside the 200 range"
        [ test "it sends a BadStatus message" <|
          \() ->
            let
              stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
                |> HttpStub.withStatus Status.notFound
            in
              Elmer.given App.defaultModel App.view App.update
                |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
                |> Markup.target << by [ id "request-data-click" ]
                |> Event.click
                |> Markup.target << by [ id "data-result" ]
                |> Markup.expect (element <| hasText "BadStatus Error: 404 Not Found")
        ]
      , describe "when the response status is in the 200 range"
        [ describe "when the response body cannot be processed"
          [ test "it fails with a message" <|
            \() ->
              let
                stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
                  |> HttpStub.withBody "{}"
              in
                Elmer.given App.defaultModel App.view App.update
                  |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
                  |> Markup.target << by [ id "request-data-click" ]
                  |> Event.click
                  |> Markup.target << by [ id "data-result" ]
                  |> Markup.expect (element <| hasText "Name: Super Fun Person")
                  |> Expect.equal (Expect.fail (format
                    [ fact "Parsing a stubbed response" "GET http://fun.com/fun.html"
                    , note ("\tWith body: \"{}\"")
                    , fact "failed with error" "Problem with the given value:\n\n{}\n\nExpecting an OBJECT with a field named `name`"
                    , note "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
                    ]
                  ))
          , describe "when the stub does not specify a body at all"
            [ test "it fails with a message" <|
              \() ->
                let
                  stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
                in
                  Elmer.given App.defaultModel App.view App.update
                    |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
                    |> Markup.target << by [ id "request-data-click" ]
                    |> Event.click
                    |> Markup.target << by [ id "data-result" ]
                    |> Markup.expect (element <| hasText "Name: Super Fun Person")
                    |> Expect.equal (Expect.fail (format
                      [ fact "Parsing a stubbed response" "GET http://fun.com/fun.html"
                      , note ("\tWith body: \"\"")
                      , fact "failed with error" "Problem with the given value:\n\n\"\"\n\nThis is not valid JSON! Unexpected end of JSON input"
                      , note "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
                      ]
                    ))
            ]
          ]
        , describe "when the requested url has a query string"
          [ test "it matches the stubbed path" <|
            \() ->
              let
                defaultModel = App.defaultModel
                stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
                  |> HttpStub.withBody "{\"name\":\"awesome things\"}"
                testModel = { defaultModel | query = "?type=awesome" }
              in
                Elmer.given testModel App.view App.update
                  |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
                  |> Markup.target << by [ id "request-data-click" ]
                  |> Event.click
                  |> Markup.target << by [ id "data-result" ]
                  |> Markup.expect (element <| hasText "awesome things")
                  |> Expect.equal Expect.pass
          ]
        , describe "when the response body can be processed"
          [ test "it decodes the response" <|
            \() ->
              let
                stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
                  |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
              in
                Elmer.given App.defaultModel App.view App.update
                  |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
                  |> Markup.target << by [ id "request-data-click" ]
                  |> Event.click
                  |> Markup.target << by [ id "data-result" ]
                  |> Markup.expect (element <| hasText "Super Fun Person")
                  |> Expect.equal Expect.pass
          ]
        , let
            stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
              |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
            otherStub = HttpStub.for (Route.get "http://fun.com/super.html")
              |> HttpStub.withBody "{\"message\":\"This is great!\"}"
            state = Elmer.given App.defaultModel App.view App.update
              |> Spy.use [ ElmerHttp.serve [ stubbedResponse, otherStub ] ]
              |> Markup.target << by [ id "request-data-click" ]
              |> Event.click
              |> Markup.target << by [ id "request-other-data-click" ]
              |> Event.click
          in
            describe "when multiple stubs are provided"
            [ test "it decodes the response for one stub" <|
              \() ->
                state
                  |> Markup.target << by [ id "data-result" ]
                  |> Markup.expect (element <| hasText "Super Fun Person")
            , test "it decodes the response for the other stub" <|
              \() ->
                state
                  |> Markup.target << by [ id "other-data-result" ]
                  |> Markup.expect (element <| hasText "This is great!")
            ]
        ]
      ]
    ]
  ]


errorResponseTests : Test
errorResponseTests =
  describe "when the request should result in an Http.Error"
  [ test "it returns the error" <|
    \() ->
      let
        stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
          |> HttpStub.withError Http.Timeout
      in
        Elmer.given App.defaultModel App.view App.update
          |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
          |> Markup.target << by [ id "request-data-click" ]
          |> Event.click
          |> Markup.target << by [ id "data-result" ]
          |> Markup.expect (element <| hasText "Timeout Error")
  ]


expectTests : Test
expectTests =
  let
    getRoute = Route.get "http://fun.com/fun.html"
  in
  describe "Http Expect"
  [ describe "when there is an upstream failure"
    [ test "it fails with the upstream failure" <|
      \() ->
        let
          stubbedResponse = HttpStub.for getRoute
        in
          Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Command.send (\_ -> Command.fail "You failed!")
            |> ElmerHttp.expectRequest getRoute
            |> Expect.equal (Expect.fail "You failed!")
    ]
  , describe "when the stub was not requested"
    [ describe "when there are no requests"
      [ test "it fails with a message" <|
        \() ->
          Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> ElmerHttp.expectRequest getRoute
            |> Expect.equal (Errors.failWith <| Errors.noRequest "GET http://fun.com/fun.html")            
      ]
    , describe "when there are other requests"
      [ test "it fails with a message" <|
        \() ->
          let
            initialState =
              testStateWithRequests
                [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                ]
          in
            ElmerHttp.expectRequest getRoute initialState
              |> Expect.equal (Errors.failWith <| 
                Errors.wrongRequest "GET http://fun.com/fun.html" "POST http://fun.com/fun\nGET http://awesome.com/awesome.html?stuff=fun"
              )
      ]
    ]
  , describe "when the stub was requested"
    [ describe "when the url matches but not the method or the method matches but not the url"
      [ test "it fails" <|
        \() ->
          let
            route = Route.get "http://fun.com/fun"
            initialState =
              testStateWithRequests
                [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                ]
          in
            ElmerHttp.expectRequest route initialState
              |> Expect.equal (Errors.failWith <| 
                Errors.wrongRequest "GET http://fun.com/fun" "POST http://fun.com/fun\nGET http://awesome.com/awesome.html?stuff=fun"
              )
      ]
    , describe "when the url and the method match"
      [ test "it passes" <|
        \() ->
          let
            initialState =
              testStateWithRequests
                [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                , (Route.get "http://fun.com/fun.html", realRequest "GET" "http://fun.com/fun.html")
                , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                ]
          in
            ElmerHttp.expectRequest getRoute initialState
              |> Expect.equal Expect.pass
      ]
    , describe "when the route and the method match"
      [ test "it passes" <|
        \() ->
          let
            initialState =
              testStateWithRequests
                [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                , (Route.get "http://fun.com/fun.html?stuff=fun", realRequest "GET" "http://fun.com/fun.html?stuff=fun")
                , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                ]
          in
            ElmerHttp.expectRequest getRoute initialState
              |> Expect.equal Expect.pass
      ]
    ]
  ]


expectThatTests : Test
expectThatTests =
  let
    getRoute = Route.get "http://fun.com/fun.html"
  in
    describe "expectThat"
    [ describe "when there is an upstream failure"
      [ test "it fails with the upstream failure" <|
        \() ->
          let
            stubbedResponse = HttpStub.for getRoute
          in
            Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
              |> Command.send (\_ -> Command.fail "You failed!")
              |> ElmerHttp.expect getRoute (\rs -> Expect.fail "NO")
              |> Expect.equal (Expect.fail "You failed!")
      ]
    , describe "when no requests have been made"
      [ test "it passes empty list to the matcher" <|
        \() ->
          let
            stubbedResponse = HttpStub.for getRoute
          in
            Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
              |> ElmerHttp.expect getRoute (\rs -> Expect.equal [] rs)
              |> Expect.equal (Expect.pass)
      ]
    , describe "when there are requests"
      [ describe "when no requests match the stub"
        [ test "it passes an empty list to the matcher" <|
          \() ->
            let
              initialState =
                testStateWithRequests
                  [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                  , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                  ]
            in
              ElmerHttp.expect getRoute (\rs -> Expect.equal [] rs) initialState
                |> Expect.equal Expect.pass
        ]
      , describe "when requests match the stub"
        [ test "it passes a list of the matching requests to the matcher" <|
          \() ->
            let
              initialState =
                testStateWithRequests
                  [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                  , (Route.get "http://fun.com/fun.html", realRequest "GET" "http://fun.com/fun.html")
                  , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                  , (Route.get "http://fun.com/fun.html", realRequest "GET" "http://fun.com/fun.html")
                  , (Route.get "http://fun.com/fun.html?stuff=fun", realRequest "GET" "http://fun.com/fun.html?stuff=fun")
                  ]
              requestForStub = testRequest "GET" "http://fun.com/fun.html"
              requestForStubQueryString = testRequest "GET" "http://fun.com/fun.html?stuff=fun"
            in
              initialState
                |> ElmerHttp.expect getRoute (\rs -> 
                  Expect.equal [ requestForStubQueryString, requestForStub, requestForStub ] rs
                ) 
        , describe "when the matcher fails"
          [ test "it fails with a message" <|
            \() ->
              let
                initialState =
                  testStateWithRequests
                    [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
                    , (Route.get "http://fun.com/fun.html", realRequest "GET" "http://fun.com/fun.html")
                    , (Route.get "http://awesome.com/awesome.html?stuff=fun", realRequest "GET" "http://awesome.com/awesome.html?stuff=fun")
                    ]
              in
                ElmerHttp.expect getRoute (\rs -> Expect.fail "Failed!") initialState
                  |> Expect.equal (Errors.failWith <|
                    Errors.requestMatcherFailed "GET http://fun.com/fun.html" "Failed!"
                  )
          ]
        ]
      ]
    ]

testStateWithRequests : List (HttpRoute, Http.Request String) -> TestState TestModel TestMsg
testStateWithRequests requests =
  Elmer.given {} testView testUpdate
    |> Spy.use [ ElmerHttp.serve <| List.map makeStub (List.map Tuple.first requests) ]
    |> Command.send (\() -> makeRequests (List.map Tuple.second requests))

makeStub : HttpRoute -> HttpResponseStub
makeStub route =
  HttpStub.for route
    |> HttpStub.withStatus Status.notFound

makeRequests : List (Http.Request String) -> Cmd TestMsg
makeRequests requests =
  List.map sendRequest requests
    |> Cmd.batch

sendRequest : Http.Request String -> Cmd TestMsg
sendRequest request =
  Http.send TestRequestTagger request

testView : TestModel -> Html TestMsg
testView model =
  Html.text ""

testUpdate : TestMsg -> TestModel -> (TestModel, Cmd TestMsg)
testUpdate msg model =
  (model, Cmd.none)

type alias TestModel =
  {}

type TestMsg
  = TestRequestTagger (Result Http.Error String)

realRequest : String -> String -> Http.Request String
realRequest method url =
  Http.request
    { method = method
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson <| Json.succeed ""
    , timeout = Nothing
    , withCredentials = False
    }


testRequest : String -> String -> HttpRequest
testRequest method url =
  { method = method
  , url = url
  , body = Nothing
  , headers = []
  }

expectRequestDataTests : Test
expectRequestDataTests =
  describe "Request Data Tests"
  [ test "it finds the headers" <|
    \() ->
      let
        stubbedResponse = 
          HttpStub.for (Route.get "http://fun.com/fun.html")
            |> HttpStub.withBody "{\"name\":\"Cool Dude\"}"
            |> HttpStub.deferResponse
      in
      Elmer.given App.defaultModel App.view App.update
        |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
        |> Markup.target << by [ id "request-data-click" ]
        |> Event.click
        |> ElmerHttp.expect (Route.get "http://fun.com/fun.html") (Elmer.some <|
            Elmer.expectAll
            [ hasHeader ("x-fun", "fun")
            , hasHeader ("x-awesome", "awesome")
            ]
        )
  ]

resolveTests : Test
resolveTests =
  let
    stubbedResponse = HttpStub.for (Route.get "http://fun.com/fun.html")
      |> HttpStub.withBody "{\"name\":\"Cool Dude\"}"
      |> HttpStub.deferResponse
    requestedState = Elmer.given App.defaultModel App.view App.update
      |> Spy.use [ ElmerHttp.serve [ stubbedResponse ] ]
      |> Markup.target << by [ id "request-data-click" ]
      |> Event.click
  in
    describe "when there is no upstream failure"
    [ describe "before resolve is called"
      [ test "it records the request" <|
        \() ->
          ElmerHttp.expectRequest (Route.get "http://fun.com/fun.html") requestedState
      , test "it does not yet resolve the response" <|
        \() ->
          requestedState
            |> Markup.target << by [ id "data-result" ]
            |> Markup.expect (element <| hasText "")
      ]
    , describe "when resolve is called"
      [ test "it resolves the response" <|
        \() ->
          requestedState
            |> Elmer.resolveDeferred
            |> Markup.target << by [ id "data-result" ]
            |> Markup.expect (element <| hasText "Cool Dude")
      ]
    ]

clearRequestsTests : Test
clearRequestsTests =
  describe "clear"
  [ describe "when there is an upstream failure"
    [ test "it shows the failure" <|
      \() ->
        let
          result = 
            Elmer.given App.defaultModel App.view App.update
              |> Command.send (\_ -> Command.fail "You Failed!")
              |> ElmerHttp.clearRequestHistory 
              |> ElmerHttp.expect (Route.post "http://fun.com/fun") (wasRequested 0)
        in
          Expect.equal (Expect.fail "You Failed!") result
    ]
  , describe "when there are requests to clear"
    [ test "it clears the requests" <|
      \() ->
        let
          initialState =
            testStateWithRequests
              [ (Route.post "http://fun.com/fun", realRequest "POST" "http://fun.com/fun")
              , (Route.get "http://fun.com/fun.html", realRequest "GET" "http://fun.com/fun.html")
              ]
        in
          initialState
            |> ElmerHttp.clearRequestHistory
            |> Elmer.expectAll
              [ ElmerHttp.expect (Route.post "http://fun.com/fun") (wasRequested 0)
              , ElmerHttp.expect (Route.get "http://fun.com/fun.html") (wasRequested 0)
              ]
    ]
  ]
