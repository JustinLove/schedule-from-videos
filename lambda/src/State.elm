module State exposing
  ( Retry(..)
  , Request(..)
  , State
  , fetchVideos
  )

import Json.Encode exposing (Value)

type Retry
  = Retried
  | WillRetry

type Request
  = FetchVideos { userId: String}

type alias State =
  { request : Request
  , shouldRetry : Retry
  , session : Value
  }

fetchVideos : String -> Value -> State
fetchVideos userId session =
  { request = FetchVideos {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }
