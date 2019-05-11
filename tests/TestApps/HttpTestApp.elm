module TestApps.HttpTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Task exposing (Task)

type alias Model =
  { dataResult : String
  , otherDataResult : String
  , query : String
  }

defaultModel : Model
defaultModel =
  { dataResult = ""
  , otherDataResult = ""
  , query = ""
  }

type Msg
  = RequestData
  | RequestDataWithTask
  | WebServiceResponse (Result Http.Error String)
  | RequestOtherData
  | RequestOtherDataWithTask
  | OtherWebServiceResponse (Result Http.Error String)

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
  [ Html.div [ Attr.id "request-data-click", onClick RequestData ]
    [ Html.text "Click for data" ]
  , Html.div [ Attr.id "request-data-with-task-click", onClick RequestDataWithTask ]
    [ Html.text "Click for data with task" ]
  , Html.div [ Attr.id "data-result" ]
    [ Html.text model.dataResult ]
  , Html.div [ Attr.id "request-other-data-click", onClick RequestOtherData ]
    [ Html.text "Click for other data" ]
  , Html.div [ Attr.id "request-other-data-with-task-click", onClick RequestOtherDataWithTask ]
    [ Html.text "Click for other data with tasj" ]
  , Html.div [ Attr.id "other-data-result" ]
    [ Html.text model.otherDataResult ]
  ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RequestData ->
      ( model, fetchDataRequest model )
    RequestDataWithTask ->
      ( model
      , Task.sequence
          [ fetchDataRequestTask model
          , otherFetchDataRequestTask model
          ]
          |> Task.map (\dataList ->
            "Data from Http Task: " ++ (String.join " " dataList)
          )
          |> Task.attempt WebServiceResponse
      )
    WebServiceResponse (Ok name) ->
      ( { model | dataResult = name }, Cmd.none )
    WebServiceResponse (Err (Http.BadBody message)) ->
      ( { model | dataResult = "BadBody Error: " ++ message }, Cmd.none )
    WebServiceResponse (Err (Http.BadStatus code)) ->
      ( { model | dataResult = ("BadStatus Error: " ++ (String.fromInt code)) }, Cmd.none )
    WebServiceResponse (Err Http.Timeout) ->
      ( { model | dataResult = "Timeout Error" }, Cmd.none )
    WebServiceResponse (Err Http.NetworkError) ->
      ( { model | dataResult = "Network Error" }, Cmd.none )
    WebServiceResponse (Err (Http.BadUrl message)) ->
      ( { model | dataResult = "BadUrl Error: " ++ message }, Cmd.none )
    RequestOtherData ->
      ( model, otherFetchDataRequest model )
    RequestOtherDataWithTask ->
      ( model
      , fetchDataRequestTask model
          |> Task.andThen anotherRequestTask
          |> Task.map (\data ->
            "Data from Task: " ++ data
          )
          |> Task.attempt OtherWebServiceResponse
      )
    OtherWebServiceResponse (Ok message) ->
      ( { model | otherDataResult = message }, Cmd.none )
    OtherWebServiceResponse (Err _) ->
      ( { model | otherDataResult = "Error!" }, Cmd.none )


fetchDataRequest : Model -> Cmd Msg
fetchDataRequest model =
  Http.request
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = ("http://fun.com/fun.html" ++ model.query)
    , body = Http.emptyBody
    , expect = Http.expectJson WebServiceResponse webServiceDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

fetchDataRequestTask : Model -> Task Http.Error String
fetchDataRequestTask model =
  Http.task
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = ("http://fun.com/fun.html" ++ model.query)
    , body = Http.emptyBody
    , resolver = Http.stringResolver <| decodeResponse webServiceDecoder
    , timeout = Nothing
    }

decodeResponse : Json.Decoder String -> Http.Response String -> Result Http.Error String
decodeResponse decoder response =
  case response of
    Http.GoodStatus_ metadata body ->
      Json.decodeString decoder body
        |> Result.mapError Json.errorToString
        |> Result.mapError Http.BadBody
    Http.Timeout_ ->
      Err <| Http.Timeout
    _ ->
      Err <| Http.BadBody <| "Some unhandled error"


otherFetchDataRequest : Model -> Cmd Msg
otherFetchDataRequest model =
  Http.request
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = "http://fun.com/super.html"
    , body = Http.emptyBody
    , expect = Http.expectJson OtherWebServiceResponse otherWebServiceDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

otherFetchDataRequestTask : Model -> Task Http.Error String
otherFetchDataRequestTask model =
  Http.task
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = "http://fun.com/super.html"
    , body = Http.emptyBody
    , resolver = Http.stringResolver (decodeResponse otherWebServiceDecoder)
    , timeout = Nothing
    }

anotherRequestTask : String -> Task Http.Error String
anotherRequestTask name =
  Http.task
    { method = "POST"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = "http://fun.com/super.html?name=" ++ name
    , body = Http.jsonBody <| Encode.object [ ("name", Encode.string "Cool Dude"), ("sport", Encode.string "bowling") ]
    , resolver = Http.stringResolver (decodeResponse otherWebServiceDecoder)
    , timeout = Nothing
    }

webServiceDecoder : Json.Decoder String
webServiceDecoder =
  Json.field "name" Json.string

otherWebServiceDecoder : Json.Decoder String
otherWebServiceDecoder =
  Json.field "message" Json.string
