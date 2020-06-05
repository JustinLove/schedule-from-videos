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
  , toHttp
  )

import Lambda.Http as Http
import Reply.Encode as Encode

import Twitch.Helix.Decode as Helix

import Json.Decode as Decode
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

httpResponse : String -> (a -> Msg) -> Result Http.Error a -> Msg
httpResponse source success result =
  case result of
    Ok value -> success value
    Err (Http.BadStatus 401 body) -> AuthenticationFailed source
    Err err -> HttpError source err

validateUser : (Helix.User -> Msg) -> (List Helix.User) -> Msg
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
  , expect = Http.expectJson (httpResponse "fetchVideos" GotVideos) decodeVideos
  }

fetchVideosWithName : {userId : String, userName: String} -> Http.Request Msg
fetchVideosWithName user =
  { url = videosUrl user.userId
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchVideos" (GotVideosWithName user)) decodeVideos
  }

decodeVideos : Decode.Decoder (List Helix.Video)
decodeVideos =
  Helix.videos
    |> Decode.map (List.filter (\v -> v.videoType == Helix.Archive))

fetchUserByIdUrl : String -> String
fetchUserByIdUrl userId =
  "https://api.twitch.tv/helix/users?id=" ++ userId

fetchUserById : String -> Http.Request Msg
fetchUserById userId =
  { url =  fetchUserByIdUrl userId
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchUserById" (validateUser GotUserById)) Helix.users
  }

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> Http.Request Msg
fetchUserByName login =
  { url =  fetchUserByNameUrl login
  , method = "GET"
  , headers = []
  , expect = Http.expectJson (httpResponse "fetchUserByName" (validateUser GotUserByName)) Helix.users
  }
