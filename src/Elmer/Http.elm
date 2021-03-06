module Elmer.Http exposing
  ( HttpResponseStub
  , expectRequest
  , expect
  , clearRequestHistory
  , serve
  )

{-| Functions for handling Http requests in your tests.

Your component makes Http requests. You want to describe the behavior of your
component. What to do?

1. Create an HttpResponseStub -- see `Elmer.Http.Stub`

2. Serve it up during your test

3. Smile!

# Serve Stubbed Responses
@docs HttpResponseStub, serve

# Make Expectations about Http Requests
@docs expectRequest, expect, clearRequestHistory

-}

import Http
import Dict
import Elmer exposing (Matcher)
import Elmer.Http.Routable as Routable
import Elmer.Http.Types as Types
import Elmer.Http.Command
import Elmer.Http.Task
import Elmer.Http.Route as Route exposing (HttpRoute)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Effects as Effects
import Elmer.Command as Command
import Elmer.Message.Failure as Failure
import Elmer.Http.Errors as Errors
import Expect exposing (Expectation)
import Test.Runner


{-| Represents a stubbed HttpResponse

Use `Elmer.Http.Stub` to build an `HttpResponseStub`.
-}
type alias HttpResponseStub
  = Types.HttpResponseStub Http.Error


{-| Override `Http.send` and `Http.toTask` to register HttpResponseStubs that will be
returned when the appropriate request is received. Used in conjunction with
`Elmer.Spy.use`.

Suppose you have a component that requests information about a user when
a button is clicked. You could register a stub for that request like so

    let
      stubbedResponse = 
        Elmer.Http.Stub.for (
          Elmer.Http.Route.post "http://fun.com/user"
        ) 
          |> Elmer.Http.Stub.withBody
            "{\"name\":\"Super User\"}"
    in
      testState
        |> Spy.use [ serve [ stubbedResponse ] ]
        |> Markup.target
            << by [ id "submit-button" ]
        |> Elmer.Html.Event.click
        |> Markup.target
            << by [ id "result" ]
        |> Markup.expect (Matchers.element <| 
            Matchers.hasText "Hello, Super User!"
          )

-}
serve : List HttpResponseStub -> Spy
serve responseStubs =
  Spy.batch
    [ Spy.observe (\_ -> Http.request)
        |> andCallFake (Elmer.Http.Command.stubbedWith responseStubs)
    , Spy.observe (\_ -> Http.task)
        |> andCallFake (Elmer.Http.Task.stubbedWith responseStubs)
    ]


{-| Clear any Http requests that may have been recorded at an earlier point
in the history of this TestState.
-}
clearRequestHistory : Elmer.TestState model msg -> Elmer.TestState model msg
clearRequestHistory =
  Effects.use Types.Requests <|
    \_ ->
      Command.send (\_ -> Effects.push Types.Requests (\_ -> []))


{-| Expect one or more requests to the specified route.

If no requests have been made to the specified route, the test will fail.

Note: This must be used in conjunction with `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectRequest : HttpRoute -> Matcher (Elmer.TestState model msg)
expectRequest route =
  Effects.expect Types.Requests <|
    \maybeRequests ->
      let
        requests =
          Maybe.withDefault [] maybeRequests
      in
        if List.isEmpty requests then
          Errors.failWith <| Errors.noRequest (Routable.toString route)
        else
          case hasRequestForRoute requests route of
            Just _ ->
              Expect.pass
            Nothing ->
              let
                requestInfo =
                  List.reverse requests
                    |> List.map (\r -> r.method ++ " " ++ r.url)
                    |> String.join "\n"
              in
                Errors.failWith <| Errors.wrongRequest (Routable.toString route) requestInfo


{-| Make some expectation about requests to the specified route.

    expect (Elmer.Http.Route.get "http://fun.com/fun") (
      Elmer.each <| 
        Elmer.Http.Matchers.hasHeader 
          ("X-Auth-Token", "MY-TOKEN")
    )

If no requests have been made to the specified route, an empty list
will be passed to the `Matcher (List HttpRequest)`.

Note: This must be used in conjunction with `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expect : HttpRoute -> Matcher (List HttpRequest) -> Matcher (Elmer.TestState model msg)
expect route matcher =
  Effects.expect Types.Requests <|
    \maybeRequests ->
      let
        requests =
          Maybe.withDefault [] maybeRequests

        result =
          List.filter (Routable.matches route) requests
            |> matcher
      in
        case Test.Runner.getFailureReason result of
          Just failure ->
            Errors.failWith <| 
              Errors.requestMatcherFailed
                (Routable.toString route)
                (Failure.format [ failure ])
          Nothing ->
            Expect.pass


hasRequestForRoute : List HttpRequest -> HttpRoute -> Maybe HttpRequest
hasRequestForRoute requests route =
  List.filter (Routable.matches route) requests
    |> List.head
