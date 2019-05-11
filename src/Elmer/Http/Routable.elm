module Elmer.Http.Routable exposing
  ( matches
  , queryString
  , toString
  )


type alias Routable a =
  { a
  | url : String
  , method : String
  }


matches : Routable a -> Routable b -> Bool
matches routeA routeB =
  routeA.method == routeB.method &&
  route routeA == route routeB


route : Routable a -> String
route routable =
  String.split "?" routable.url
    |> List.head
    |> Maybe.withDefault ""


queryString : Routable a -> Maybe String
queryString routable =
  String.split "?" routable.url
    |> List.drop 1
    |> List.head


toString : Routable a -> String
toString routable = 
  routable.method ++ " " ++ routable.url