module State exposing
  ( Retry(..)
  , Request(..)
  , State
  , initVideos
  , initVideosWithName
  , initUser
  , Msg(..)
  , Effect(..)
  , update
  )

import Lambda.Http as Http
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

initVideos : String -> Value -> State
initVideos userId session =
  { request = FetchVideos {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }

initVideosWithName : String -> Value -> State
initVideosWithName userId session =
  { request = FetchVideosAndName {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }

initUser : String -> Value -> State
initUser userName session =
  { request = FetchUser {userName = userName}
  , shouldRetry = WillRetry
  , session = session
  }


type Msg
  = AuthenticationFailed String
  | HttpError String Http.Error
  | UserNotFound
  | GotVideos (List Helix.Video)
  | GotVideosWithName {userId : String, userName: String} (List Helix.Video)
  | GotUserById Helix.User
  | GotUserByName Helix.User

type Effect
 = Query State
 | AuthReset State
 | Response Value (Result String Value)

update : Msg -> State -> Effect
update msg state =
  case msg of
    AuthenticationFailed source ->
      let _ = Debug.log "auth failed " source in
      if state.shouldRetry == WillRetry then
        AuthReset {state|shouldRetry = Retried}
      else
        errorResponse state.session "unable to authenticate"
    HttpError source error ->
      let _ = Debug.log ("http error: " ++ source) error in
      errorResponse state.session "service http error"
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
