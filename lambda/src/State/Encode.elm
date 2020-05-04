module State.Encode exposing
  ( fetchVideos
  )

import State exposing (FetchVideos, Retry(..), State)

import Json.Encode exposing (..)

fetchVideos : FetchVideos -> State
fetchVideos state =
  object
    [ ("user_id", string state.userId)
    , ("should_retry", retry state.shouldRetry)
    , ("session", state.session)
    ]

retry : Retry -> Value
retry shouldRetry =
  bool (shouldRetry == WillRetry)
