port module Lambda exposing
  ( State
  , Event(..)
  , EventState(..)
  , HttpError(..)
  , event
  , decrypt
  , Header
  , header
  , HttpRequest
  , httpRequest
  , response
  )

import Secret exposing (Secret)

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode

type alias State = Value

type HttpError
  = BadStatus Int String
  | NetworkError

type EventState = EventState State Event

type Event
  = NewEvent Value
  | Decrypted (Result String (List Secret))
  | HttpResponse String (Result HttpError String)

event : (Result Decode.Error EventState -> msg) -> Sub msg
event tagger =
  lambdaEvent (decodeEvent >> tagger)

decodeEvent : Value -> Result Decode.Error EventState
decodeEvent thing =
  Decode.decodeValue eventStateDecoder thing
    |> Result.mapError (Debug.log "lambda decode error")

eventStateDecoder : Decode.Decoder EventState
eventStateDecoder =
  Decode.map2 EventState
    stateDecoder
    eventDecoder

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen(\kind ->
      case kind of
        "lambdaEvent" ->
          Decode.map NewEvent
            (Decode.field "event" Decode.value)
        "decrypted" ->
          Decode.map Decrypted
            (Decode.map Ok
              (Decode.field "values"
                (Decode.list
                  (Decode.map Secret.fromString Decode.string)
                )
              )
            )
        "decryptionError" ->
          Decode.map Decrypted
            (Decode.map Err
              (Decode.field "error" Decode.string)
            )
        "httpResponse" ->
          Decode.map2 HttpResponse
            (Decode.field "tag" Decode.string)
            (Decode.map Ok
              (Decode.field "body" Decode.string)
            )
        "badStatus" ->
          Decode.map2 HttpResponse
            (Decode.field "tag" Decode.string)
            (Decode.map Err
              (Decode.map2 BadStatus
                (Decode.field "status" Decode.int)
                (Decode.field "body" Decode.string)
              )
            )
        "networkError" ->
          Decode.map2 HttpResponse
            (Decode.field "tag" Decode.string)
            (Decode.succeed (Err NetworkError))
        _ -> Decode.fail kind
    )

stateDecoder : Decode.Decoder State
stateDecoder =
  Decode.field "state" Decode.value

decrypt : List Secret -> Cmd msg
decrypt values =
  Encode.object
    [ ("kind", Encode.string "decrypt")
    , ("values", values
      |> Encode.list (Secret.toString>>Encode.string)
      )
    ]
    |> lambdaCommand

type Header = Header String String

header : String -> String -> Header
header = Header

encodeHeader : Header -> (String, Value)
encodeHeader (Header name value) =
  (name, Encode.string value)

encodeHeaders : List Header -> Value
encodeHeaders = (List.map encodeHeader) >> Encode.object

type alias HttpRequest =
  { hostname : String
  , path : String
  , method : String
  , headers : List Header
  , tag : String
  }

httpRequest : HttpRequest -> Value -> Cmd msg
httpRequest req state =
  Encode.object
    [ ("kind", Encode.string "httpRequest")
    , ("state", state)
    , ("request", Encode.object
      [ ("hostname", Encode.string req.hostname)
      , ("path", Encode.string req.path)
      , ("method", Encode.string req.method)
      , ("headers", encodeHeaders req.headers)
      , ("tag", Encode.string req.tag)
      ])
    ]
    |> lambdaCommand

response : Value -> Result String Value -> Cmd msg
response session result =
  case result of
    Ok value -> successResponse session value
    Err err -> errorResponse session err

errorResponse : Value -> String -> Cmd msg
errorResponse session error =
  Encode.object
    [ ("kind", Encode.string "error")
    , ("session", session)
    , ("error", Encode.string error)
    ]
    |> lambdaCommand

successResponse : Value -> Value -> Cmd msg
successResponse session data =
  Encode.object
    [ ("kind", Encode.string "success")
    , ("session", session)
    , ("data", data)
    ]
    |> lambdaCommand

port lambdaCommand : Value -> Cmd msg
port lambdaEvent : (Value -> msg) -> Sub msg
