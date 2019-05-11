module Elmer.Http.Internal exposing
  ( isGoodStatusCode
  , route
  , queryString
  , routeToString
  )


isGoodStatusCode : Int -> Bool
isGoodStatusCode code =
  code >= 200 && code < 300


route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""


queryString : String -> Maybe String
queryString url =
  String.split "?" url
    |> List.drop 1
    |> List.head


type alias Routable a =
  { a
  | url : String
  , method : String
  }


routeToString : Routable a -> String
routeToString routable = 
  routable.method ++ " " ++ routable.url