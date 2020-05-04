module State exposing
  ( Retry(..)
  , FetchVideos
  , State
  , fetchVideos
  )

import Json.Encode exposing (Value)

type alias State = Value

type Retry
  = Retried
  | WillRetry

type alias FetchVideos =
  { userId : String
  , shouldRetry : Retry
  , session : Value
  }

fetchVideos : String -> Value -> FetchVideos
fetchVideos userId session =
  { userId = userId
  , shouldRetry = WillRetry
  , session = session
  }
