port module TwitchExt exposing (Auth, Context, onAuthorized, onContext, log)

import Json.Decode as Decode
import Json.Encode as Encode
import Http

port onAuthorized : (Auth -> msg) -> Sub msg
port onContext : (Context -> msg) -> Sub msg
port onError : (Decode.Value -> msg) -> Sub msg

port logCommand : Decode.Value -> Cmd msg

type alias Auth =
  { channelId : String
  , clientId : String
  , token : String
  , userId : String
  }

auth : Decode.Decoder Auth
auth =
  Decode.map4 Auth
    (Decode.field "channelId" Decode.string)
    (Decode.field "clientId" Decode.string)
    (Decode.field "token" Decode.string)
    (Decode.field "userId" Decode.string)

type alias Context =
  { theme : String
  }

context : Decode.Decoder Context
context =
  Decode.map Context
    (Decode.field "theme" Decode.string)

decodeError : String -> Decode.Error -> Cmd msg
decodeError note err =
  err
    |> Decode.errorToString
    |> Encode.string
    |> log note

httpError : String -> Http.Error -> Cmd msg
httpError note err =
  (case err of
    Http.BadUrl url ->
      Encode.object
        [ ("error", Encode.string "BadUrl")
        , ("url", Encode.string url)
        ]
    Http.Timeout ->
      Encode.object
        [ ("error", Encode.string "Timeout")
        ]
    Http.NetworkError ->
      Encode.object
        [ ("error", Encode.string "NetworkError")
        ]
    Http.BadStatus status ->
      Encode.object
        [ ("error", Encode.string "BadStatus")
        , ("status", Encode.int status)
        ]
    Http.BadBody message ->
      Encode.object
        [ ("error", Encode.string "BadBody")
        , ("message", Encode.string message)
        ]
  )
    |> log note

log : String -> Encode.Value -> Cmd msg
log note value =
  Encode.object
    [ ("kind", Encode.string "log")
    , ("note", Encode.string note)
    , ("value", value)
    ]
    |> logCommand
