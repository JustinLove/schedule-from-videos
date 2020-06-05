port module Lambda.Port exposing
  ( Session
  , Event(..)
  , HttpResponse(..)
  , event
  , decrypt
  , Header
  , header
  , httpRequest
  , response
  )

import Secret exposing (Secret)

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode

type alias Session = Value

type HttpResponse
  = BadUrl String
  | NetworkError
  | BadStatus Int String
  | GoodStatus String

type Event
  = SystemError String
  | NewEvent Value Session
  | Decrypted (Result String (List Secret))
  | HttpResponse Int HttpResponse

event : (Event -> msg) -> Sub msg
event tagger =
  lambdaEvent (decodeEvent >> tagger)

decodeEvent : Value -> Event
decodeEvent thing =
  case Decode.decodeValue eventDecoder thing of
    Ok ev ->
      ev
    Err err ->
      err
        |> Debug.log "lambda decode error"
        |> Decode.errorToString
        |> SystemError

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen(\kind ->
      case kind of
        "lambdaEvent" ->
          Decode.map2 NewEvent
            (Decode.field "event" Decode.value)
            (Decode.field "session" Decode.value)
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
            (Decode.field "id" Decode.int)
            (Decode.map GoodStatus
              (Decode.field "body" Decode.string)
            )
        "badStatus" ->
          Decode.map2 HttpResponse
            (Decode.field "id" Decode.int)
            (Decode.map2 BadStatus
              (Decode.field "status" Decode.int)
              (Decode.field "body" Decode.string)
            )
        "networkError" ->
          Decode.map2 HttpResponse
            (Decode.field "id" Decode.int)
            (Decode.succeed NetworkError)
        _ -> Decode.fail kind
    )

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

type alias HttpRequestArgument =
  { hostname : String
  , path : String
  , method : String
  , headers : List Header
  , id : Int
  }

httpRequest : HttpRequestArgument -> Cmd msg
httpRequest req =
  Encode.object
    [ ("kind", Encode.string "httpRequest")
    , ("request", Encode.object
      [ ("hostname", Encode.string req.hostname)
      , ("path", Encode.string req.path)
      , ("method", Encode.string req.method)
      , ("headers", encodeHeaders req.headers)
      , ("id", Encode.int req.id)
      ])
    ]
    |> lambdaCommand

response : Session -> Result String Value -> Cmd msg
response session result =
  case result of
    Ok value -> successResponse session value
    Err err -> errorResponse session err

errorResponse : Session -> String -> Cmd msg
errorResponse session error =
  Encode.object
    [ ("kind", Encode.string "error")
    , ("session", session)
    , ("error", Encode.string error)
    ]
    |> lambdaCommand

successResponse : Session -> Value -> Cmd msg
successResponse session data =
  Encode.object
    [ ("kind", Encode.string "success")
    , ("session", session)
    , ("data", data)
    ]
    |> lambdaCommand

port lambdaCommand : Value -> Cmd msg
port lambdaEvent : (Value -> msg) -> Sub msg
