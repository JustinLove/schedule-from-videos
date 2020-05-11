module State.Encode exposing
  ( state
  )

import State exposing (Retry(..), Request(..), State)

import Json.Encode exposing (..)

state : State -> Value
state s =
  object
    [ ("request", request s.request)
    , ("should_retry", retry s.shouldRetry)
    , ("session", s.session)
    ]

request : Request -> Value
request r =
  case r of
    FetchVideos {userId} ->
      object
        [ ("request", string "fetchVideos")
        , ("user_id", string userId)
        ]
    FetchVideosAndName {userId} ->
      object
        [ ("request", string "fetchVideosAndName")
        , ("user_id", string userId)
        ]
    FetchVideosWithName {userId,userName} ->
      object
        [ ("request", string "fetchVideosWithName")
        , ("user_id", string userId)
        , ("user_name", string userName)
        ]

retry : Retry -> Value
retry shouldRetry =
  bool (shouldRetry == WillRetry)
