module State exposing
  ( Retry(..)
  , Request(..)
  , State
  , init
  , Msg(..)
  , Effect(..)
  , update
  , toHttp
  )

import Decode as Decode
import Event.Decode as Event
import Lambda.Http as Http
import Reply.Encode as Encode

import Json.Decode as Decode exposing (Value)

type alias Session = Value

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
  , session : Session
  }

initVideos : String -> Session -> State
initVideos userId session =
  { request = FetchVideos {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }

initVideosWithName : String -> Session -> State
initVideosWithName userId session =
  { request = FetchVideosAndName {userId = userId}
  , shouldRetry = WillRetry
  , session = session
  }

initUser : String -> Session -> State
initUser userName session =
  { request = FetchUser {userName = userName}
  , shouldRetry = WillRetry
  , session = session
  }

init : Value -> Session -> Effect
init data session =
  case Decode.decodeValue Event.event data of
    Ok ev ->
      Query (stateForEvent ev session)
    Err err ->
      let _ = Debug.log "event error" err in
      errorResponse session "unrecognized event"

stateForEvent : Event.Event -> Session -> State
stateForEvent event session =
  case event of
    Event.Videos {userId} ->
      initVideos userId session
    Event.VideosWithName {userId} ->
      initVideosWithName userId session
    Event.User {userName} ->
      initUser userName session

type Msg
  = AuthenticationFailed String
  | HttpError String Http.Error
  | UserNotFound
  | GotVideos (List Decode.Event)
  | GotVideosWithName {userId : String, userName: String} (List Decode.Event)
  | GotUserById Decode.User
  | GotUserByName Decode.User

type Effect
 = Query State
 | AuthReset State
 | Response Session (Result String Value)

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

errorResponse : Session -> String -> Effect
errorResponse session reason =
  Response session (Err reason)

sendResponse : Session -> Value -> Effect
sendResponse session response =
  Response session (Ok response)

httpResponse : String -> (a -> Msg) -> Result Http.Error a -> Msg
httpResponse source success result =
  case result of
    Ok value -> success value
    Err (Http.BadStatus 401 body) -> AuthenticationFailed source
    Err err -> HttpError source err

validateUser : (Decode.User -> Msg) -> (List Decode.User) -> Msg
validateUser success users =
  case users of
    user :: _ -> success user
    [] -> UserNotFound

toHttp : Request -> Http.Request Msg
toHttp request =
  case request of
    FetchVideos {userId} ->
      fetchVideos userId
    FetchVideosAndName {userId} ->
      fetchUserById userId
    FetchVideosWithName user ->
      fetchVideosWithName user
    FetchUser {userName} ->
      fetchUserByName userName

videosUrl : String -> String
videosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : String -> Http.Request Msg
fetchVideos userId =
  { url = videosUrl userId
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchVideos" GotVideos) Decode.events
  }

fetchVideosWithName : {userId : String, userName: String} -> Http.Request Msg
fetchVideosWithName user =
  { url = videosUrl user.userId
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchVideos" (GotVideosWithName user)) Decode.events
  }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl userId =
  "https://api.twitch.tv/helix/users?id=" ++ userId

fetchUserById : String -> Http.Request Msg
fetchUserById userId =
  { url =  fetchUserByIdUrl userId
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchUserById" (validateUser GotUserById)) Decode.users
  }

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> Http.Request Msg
fetchUserByName login =
  { url =  fetchUserByNameUrl login
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchUserByName" (validateUser GotUserByName)) Decode.users
  }
