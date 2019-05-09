module Elmer.Http.Internal exposing
  ( isGoodStatusCode
  , route
  , queryString
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
