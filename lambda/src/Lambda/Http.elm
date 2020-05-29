module Lambda.Http exposing
  ( Error(..)
  , publicError
  , Expect
  , expectJson
  , decodeResponse
  , Request
  , RequestId
  , httpMatch
  , rememberHttpRequest
  )

import Lambda.Port as Port

import Dict exposing (Dict)
import Json.Decode as Decode

type Error
  = BadStatus Int String
  | NetworkError
  | BadBody Decode.Error

publicError : Port.HttpError -> Error
publicError error =
  case error of
    Port.BadStatus status body -> BadStatus status body
    Port.NetworkError -> NetworkError

type Expect msg
  = ExpectString (Result Error String -> msg)

expectJson : (Result Error a -> msg) -> Decode.Decoder a -> Expect msg
expectJson tagger decoder =
  ExpectString (Result.andThen (Decode.decodeString decoder >> Result.mapError BadBody)
    >> tagger
  )

decodeResponse : Expect msg -> Result Error String -> msg
decodeResponse expect response =
  case expect of
    ExpectString decodeTagger ->
      decodeTagger response

type alias Request msg =
  { hostname : String
  , path : String
  , method : String
  , headers : List Port.Header
  , expect : Expect msg
  }

type alias RequestId = Int

type alias HttpModel model msg =
  { model
  | nextRequestId : RequestId
  , outstandingRequests : Dict Int (Expect msg)
  }

httpMatch : Int -> HttpModel model msg -> (Expect msg, HttpModel model msg)
httpMatch id model =
  case Dict.get id model.outstandingRequests of
    Just expect ->
      (expect, { model | outstandingRequests = Dict.remove id model.outstandingRequests })
    Nothing ->
      Debug.todo "response to unknown request"

rememberHttpRequest : Request appMsg -> HttpModel model appMsg -> (HttpModel model appMsg, Cmd msg)
rememberHttpRequest req model =
  let
    id = model.nextRequestId + 1
  in
  ( { model
    | nextRequestId = id
    , outstandingRequests = Dict.insert id req.expect model.outstandingRequests
    }
  , toLambdaRequest id req
  )

toLambdaRequest : RequestId -> Request appMsg -> Cmd msg
toLambdaRequest id req =
  Port.httpRequest
    { hostname = req.hostname
    , path = req.path
    , method = req.method
    , headers = req.headers
    , id = id
    }

