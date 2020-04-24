port module Lambda exposing
  ( Event(..)
  , event
  , Header
  , header
  , request
  )

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode

type HttpError
  = BadStatus Int
  | BadBody String
  | NetworkError

type Event
  = Videos String
  | Response (Result HttpError Decode.Value)

event : (Result Decode.Error Event -> msg) -> Sub msg
event tagger =
  lambdaEvent (decodeEvent >> tagger)

decodeEvent : Value -> Result Decode.Error Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing
    |> Result.mapError (Debug.log "lambda decode error")

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen(\kind ->
      case Debug.log "kind" kind of
        "lambdaEvent" ->
          Decode.map Videos
            (Decode.at ["event", "user_id"] Decode.string)
        "response" ->
          Decode.map (Response<<Ok)
            (Decode.field "body" Decode.value)
        "badStatus" ->
          Decode.map (Response<<Err<<BadStatus)
            (Decode.field "status" Decode.int)
        "badBody" ->
          Decode.map (Response<<Err<<BadBody)
            (Decode.field "error" Decode.string)
        "networkError" ->
          Decode.succeed (Response (Err NetworkError))
        _ -> Decode.fail kind
    )

type Header = Header String String

header : String -> String -> Header
header = Header

encodeHeader : Header -> (String, Value)
encodeHeader (Header name value) =
  (name, Encode.string value)

encodeHeaders : List Header -> Value
encodeHeaders = (List.map encodeHeader) >> Encode.object

request :
  { hostname : String
  , path : String
  , method : String
  , headers : List Header
  }
  -> Cmd msg
request req =
  Encode.object
    [ ("kind", Encode.string "request")
    , ("request", Encode.object
      [ ("hostname", Encode.string req.hostname)
      , ("path", Encode.string req.path)
      , ("method", Encode.string req.method)
      , ("headers", encodeHeaders req.headers)
      ])
    ]
    |> lambdaCommand

port lambdaCommand : Value -> Cmd msg
port lambdaEvent : (Value -> msg) -> Sub msg
