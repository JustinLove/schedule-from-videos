module State exposing
  ( Retry(..)
  , Request(..)
  , State
  , fetchVideos
  , fetchVideosWithName
  , fetchUser
  , Msg(..)
  , Effect(..)
  , update
  )

import Reply.Encode as Encode

import Twitch.Helix.Decode as Helix

import Json.Encode exposing (Value)

type Retry
  = Retried
  | WillRetry

type Request
  = FetchVideos { userId: String }
  | FetchVideosAndName { userId: String }
  | FetchVideosWithName { userId: String, userName: String }
  | FetchUser { userName: String }

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

fetchUser : String -> Value -> State
fetchUser userName session =
  { request = FetchUser {userName = userName}
  , shouldRetry = WillRetry
  , session = session
  }


type Msg
  = AuthenticationFailed String
  | UserNotFound
  | GotVideos (List Helix.Video)
  | GotVideosWithName {userId : String, userName: String} (List Helix.Video)
  | GotUserById Helix.User
  | GotUserByName Helix.User

type Effect
 = Query State
 | Response Value (Result String Value)

update : Msg -> State -> Effect
update msg state =
  case msg of
    AuthenticationFailed source ->
      let _ = Debug.log "auth failed " source in
      if state.shouldRetry == WillRetry then
        Query {state|shouldRetry = Retried}
      else
        errorResponse state.session "unable to authenticate"
    UserNotFound ->
      errorResponse state.session "user not found"
    GotVideos videos ->
      Encode.videosReply {events = videos}
        |> sendResponse state.session
    GotVideosWithName {userId, userName} videos ->
      Encode.videosWithNameReply
        { user = { id = userId, name = userName }
        , events = videos
        }
        |> sendResponse state.session
    GotUserById user ->
      Query
        {state|request = FetchVideosWithName
          { userId = user.id
          , userName = user.displayName
          }
        }
    GotUserByName user ->
      Encode.userReply { user = {id = user.id, name = user.displayName } }
        |> sendResponse state.session

errorResponse : Value -> String -> Effect
errorResponse session reason =
  Response session (Err reason)

sendResponse : Value -> Value -> Effect
sendResponse session response =
  Response session (Ok response)
