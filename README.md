# Elmer.Http

This is an extension for [Elmer](https://github.com/brian-watkins/elmer) that adds test functions and
matchers for describing the behavior of Elm apps that use HTTP.


### Requirements

- [Elmer](https://github.com/brian-watkins/elmer) - Version 6.x
- [elm/http](https://github.com/elm/http) - Version 2.x

If you're interested in using Elmer.Http with elm/http version 1.0.0 then you'll need the
1.0.0 release of this package.


### Getting Started

First, you need to install Elmer 6.x in your project. See the documentation
for [Elmer](https://github.com/brian-watkins/elmer) for more information on installing the latest version.

Next, you should clone this repository and copy or link the src files into your tests directory.


### Documentation

Read the latest docs [here](https://elmer-test.cfapps.io/elmer.http).


### Releases

#### 2.0.0
- Basic support for elm/http 2.0.0
- Use `Http.get`, `Http.post`, `Http.request`, and `Http.Task` during a test.

#### 1.0.0
- Basic support for elm/http 1.0.0
- Use `Http.send` and `Http.toTask` during a test.


### Usage

Modern web apps often need to make HTTP requests to some backend server. Elmer makes it easy to stub HTTP
responses and write expectations about the requests made. The `Elmer.Http.Stub` module contains methods
for constructing an `HttpResponseStub` that describes how to respond to some request. For example,
we might stub a request to the server for our game to return high scores like so:

```
let
  stubbedResponse = Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fakeGameServer.com/scores")
    |> Elmer.Http.Stub.withBody "[{\"score\":700,\"player\":\"Brian\"},{\"score\":900,\"player\":\"Holly\"}]"
in
```

In this case, a GET request to the given route will result in a response with the given body.
See `Elmer.Http.Stub` for the full list of builder functions.

Once an `HttpResponseStub` has been created, you can use the `Elmer.Http.serve` function
along with `Elmer.Spy.use` to override `Http.send` and `Http.toTask` from [elm/http](https://package.elm-lang.org/packages/elm/http/latest/) during your test.
When your application code calls `Http.send` or `Http.toTask`, the request will be checked against the
provided stubs and if a match occurs, the given response will be returned.

Here's how we can extend our test to describe more of its pre-conditions:

```
allTests : Test
allTests =
  describe "My Fun Game"
  [ describe "High Score Screen"
    [ test "it shows the high scores" <|
      \() ->
        let
          stubbedResponse =
            Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fakeGameServer.com/scores")
              |> Elmer.Http.Stub.withBody 
                "[{\"score\":700,\"player\":\"Brian\"},{\"score\":900,\"player\":\"Holly\"}]"
        in
          Elmer.Program.givenElement App.view App.update
            |> Elmer.Spy.use [ Elmer.Http.serve [ stubbedResponse ] ]
            |> Elmer.Program.init (\_ -> App.init testFlags)
            |> Elmer.Html.target
                << Elmer.Html.Selector.childrenOf 
                  [ Elmer.Html.Selector.tag "ol"
                  , Elmer.Html.Selector.class "score-list"
                  ]
                << Elmer.Html.Selector.by
                  [ Elmer.Html.Selector.tag "li" ]
            |> Elmer.Html.expect (Elmer.Html.Matchers.elements <|
                Elmer.expectAll
                [ Elmer.hasLength 2
                , Elmer.atIndex 0 <| Elmer.Html.Matchers.hasText "700 Points"
                , Elmer.atIndex 1 <| Elmer.Html.Matchers.hasText "900 Points"
                ]
              )
    ]
  ]
```

Elmer also allows you to write tests that expect some HTTP request to have been made, in a
manner similar to how you can write expectations about some element in an HTML document. For
example, this test inputs search terms into a field, clicks a search button, and then expects
that a request is made to a specific route with the search terms in the query string:

```
Elmer.given App.defaultModel App.view App.update
  |> Elmer.Spy.use [ Elmer.Http.serve [ stubbedResponse ] ]
  |> Elmer.Html.target << by [ tag "input", attribute ("name", "query") ]
  |> Elmer.Html.Event.input "Fun Stuff"
  |> Elmer.Html.target << by [ id "search-button" ]
  |> Elmer.Html.Event.click
  |> Elmer.Http.expect (Elmer.Http.Route.get "http://fake.com/search") (
    Elmer.some <| Elmer.Http.Matchers.hasQueryParam ("q", "Fun Stuff")
  )
```

If you don't care to describe the behavior of your app after the response from a request is
received -- that is, if you don't care to create a stubbed response for some request -- you
can provide `Elmer.Http.spy` to `Elmer.Spy.use` and it will override the `Http.send` and `Http.toTask`
functions so that they merely record any requests received.

See `Elmer.Http` and `Elmer.Http.Matchers` for more.


#### Deferred Commands

It's often necessary to describe the behavior of an application while some command is running. For example,
one might want to show a progress indicator while an HTTP request is in process. Elmer provides
general support for deferred commands. Use `Elmer.Command.defer` to create a command that
will not be processed until `Elmer.resolveDeferred` is called. Note that all currently
deferred commands will be resolved when this function is called.

`Elmer.Http` allows you to specify when the processing of a stubbed response should be deferred.
When you create your `HttpResponseStub` just use the `Elmer.Http.Stub.deferResponse` builder function
to indicate that this response should be deferred until `Elmer.resolveDeferred` is called.
