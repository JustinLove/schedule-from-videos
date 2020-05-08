module State.Decode exposing
  ( decodeState
  )

import State exposing (Retry(..), Request(..), State)

import Json.Decode exposing (..)

decodeState : Value -> Result Error State
decodeState =
  decodeValue state

state : Decoder State
state =
  map3 State
    (field "request" request)
    (field "should_retry" retry)
    (field "session" value)

request : Decoder Request
request =
  (field "request" string)
    |> andThen (\req ->
      case req of
        "fetchVideos" ->
          (field "user_id" string)
            |> map (\u -> {userId = u})
            |> map FetchVideos
        _ -> fail "unknown request state"
      )

retry : Decoder Retry
retry =
  bool
    |> andThen (\b -> succeed (if b then WillRetry else Retried))
