module State exposing
  ( Retry(..)
  , Request(..)
  , State
  , fetchVideos
  , fetchVideosWithName
  )

import Json.Encode exposing (Value)

type Retry
  = Retried
  | WillRetry

type Request
  = FetchVideos { userId: String }
  | FetchVideosAndName { userId: String }
  | FetchVideosWithName { userId: String, userName: String }

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

fetchVideosWithName : String -> Value -> State
fetchVideosWithName userId session =
  { request = FetchVideosAndName {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }
