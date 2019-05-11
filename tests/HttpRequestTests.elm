module HttpRequestTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Http.Adapter as HttpAdapter
import Elmer.Http.Adapter.Request as RequestAdapter
import Elmer.Http.Request as HttpRequest exposing (HttpRequest)
import Http
import Dict


bodyTests : Test
bodyTests =
  describe "body"
  [ describe "when the body is Nothing"
    [ test "it returns Nothing" <|
      \() ->
        HttpRequest.body (testRequest Nothing)
          |> Expect.equal Nothing
    ]
  , describe "when the body is not Nothing"
    [ test "it returns the body" <|
      \() ->
        HttpRequest.body (testRequest <| Just "Something")
          |> Expect.equal (Just "Something")
    ]
  ]

queryStringTest : Test
queryStringTest =
  describe "queryString"
  [ describe "when there is no query string"
    [ test "it returns Nothing" <|
      \() ->
        HttpRequest.queryString (testRequest <| Just "Something")
          |> Expect.equal Nothing
    ]
  , describe "when there is a query string"
    [ test "it returns the query string" <|
      \() ->
        HttpRequest.queryString (testQueryRequest "fruit=apple&sport=bowling")
          |> Expect.equal (Just "fruit=apple&sport=bowling")
    ]
  ]

headersTest : Test
headersTest =
  describe "headers"
  [ describe "when there are no headers"
    [ test "it returns an empty list" <|
      \() ->
        HttpRequest.headers (testRequest <| Nothing)
          |> Expect.equal []
    ]
  , describe "when there are headers"
    [ test "it returns a list of tuples" <|
      \() ->
        let
          headers =
            [ { name = "x-header-1", value = "x-value-1" }
            , { name = "x-header-2", value = "x-value-2" }
            ]
        in
          HttpRequest.headers (testHeadersRequest headers)
            |> Expect.equal [ ("x-header-1", "x-value-1"), ("x-header-2", "x-value-2") ]
    ]
  ]

type TestMsg
  = StringResult (Result Http.Error String)

testRequest : Maybe String -> HttpRequest
testRequest body =
  RequestAdapter.decode
    { method = "GET"
    , headers = []
    , url = "http://fake.com"
    , body = 
      case body of
        Just value ->
          Http.stringBody "application/json" value
        Nothing ->
          Http.emptyBody
    , expect = Http.expectString StringResult
    , timeout = Nothing
    , tracker = Nothing
    }


testQueryRequest : String -> HttpRequest
testQueryRequest query =
  RequestAdapter.decode
    { method = "GET"
    , headers = []
    , url = "http://fake.com?" ++ query
    , body = Http.emptyBody
    , expect = Http.expectString StringResult
    , timeout = Nothing
    , tracker = Nothing
    }


testHeadersRequest : List { name : String, value : String } -> HttpRequest
testHeadersRequest headers =
  RequestAdapter.decode
    { method = "GET"
    , headers = List.map (\h -> Http.header h.name h.value) headers
    , url = "http://fake.com"
    , body = Http.emptyBody
    , expect = Http.expectString StringResult
    , timeout = Nothing
    , tracker = Nothing
    }
