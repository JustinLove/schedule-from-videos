module State.Decode exposing
  ( fetchVideos
  )

import State exposing (FetchVideos, Retry(..), State)

import Json.Decode exposing (..)

fetchVideos : State -> Result Error FetchVideos
fetchVideos =
  decodeValue fetchVideosDecoder

fetchVideosDecoder : Decoder FetchVideos
fetchVideosDecoder =
  map3 FetchVideos
    (field "user_id" string)
    (field "should_retry" retry)
    (field "session" value)

retry : Decoder Retry
retry =
  bool
    |> andThen (\b -> succeed (if b then WillRetry else Retried))
