module Handler exposing (main)

import Env exposing (Env)
import Event.Decode as Event
import Lambda exposing (Effect)
import Lambda.Port as Port
import Lambda.Http as Http
import Reply.Encode as Encode
import Secret exposing (Secret)
import State exposing (Retry(..), Request(..), State)
--import State.Decode as Decode
--import State.Encode as Encode

import Twitch.Id.Decode as Id
import Twitch.Helix.Decode as Helix

import Json.Decode as Decode exposing (Value)

type alias Model =
  { env : Env
  , auth : Maybe Secret
  , pendingRequests : List State
  }

type Msg
  = NewEvent Value Lambda.Session
  | Decrypted (Result String Env)
  | GotToken (Result Http.Error Secret)
  | HttpError State String Http.Error
  | WithState State State.Msg

main = Lambda.program
  { init = appInit
  , update = appUpdate
  , newEvent = NewEvent
  , decrypted = decrypted
  }

appInit : Value -> (Model, Effect Msg)
appInit flags =
  case (Env.decode flags) of
    Ok env -> (initialModel env, Lambda.NoEffect)
    Err err -> Debug.todo ("env decode error" ++ (Debug.toString err))

initialModel : Env -> Model
initialModel env =
  { env = env
  , auth = Nothing
  , pendingRequests = []
  }

appUpdate : Msg -> Model -> (Model, Effect Msg)
appUpdate msg model =
  case msg of
    NewEvent data session ->
      case Decode.decodeValue Event.event data of
        Ok ev ->
          appendState (stateForEvent ev session) model
        Err err ->
          let _ = Debug.log "event error" err in
          (model, errorResponse session "unrecognized event")
    Decrypted (Ok env) ->
      { model | env = env }
        |> step
    Decrypted (Err err) ->
      let _ = Debug.log ("decrypt error ") err in
      withAllRequests (errorResponseState "service misconfiguration") model
    GotToken (Ok auth) ->
      { model | auth = Just auth }
        |> step
    GotToken (Err err) ->
      let _ = Debug.log "unable to fetch token" err in
      withAllRequests (errorResponseState "unable to fetch token") model
    HttpError state source error ->
      let _ = Debug.log ("http error: " ++ source) error in
      (model, errorResponse state.session "service http error")
    WithState state stateMsg ->
      case State.update stateMsg state of
        State.Query newState ->
          model |> appendState newState
        State.Response session result ->
          (model, Lambda.Response session result)

decrypted : Result String (List Secret) -> Msg
decrypted result =
  result
    |> Result.andThen decryptToEnv
    |> Decrypted

decryptToEnv : List Secret -> Result String Env
decryptToEnv list =
  case list of
    [id, secret] ->
      Env.Plain
        { clientId = id
        , clientSecret = secret
        }
        |> Ok
    _ ->
      Err ("decrypt wrong number of arguments" ++ (List.length list |> String.fromInt))

httpResponse : State -> String -> (a -> State.Msg) -> Result Http.Error a -> Msg
httpResponse state source success result =
  case result of
    Ok value -> WithState state (success value)
    Err (Http.BadStatus 401 body) -> WithState state (State.AuthenticationFailed source)
    Err err -> HttpError state source err

validateUser : (Helix.User -> State.Msg) -> (List Helix.User) -> State.Msg
validateUser success users =
  case users of
    user :: _ -> success user
    [] -> State.UserNotFound

stateForEvent : Event.Event -> Lambda.Session -> State
stateForEvent event session =
  case event of
    Event.Videos {userId} ->
      State.fetchVideos userId session
    Event.VideosWithName {userId} ->
      State.fetchVideosWithName userId session
    Event.User {userName} ->
      State.fetchUser userName session

step : Model -> (Model, Effect Msg)
step model =
  case model.env of
    Env.Plain env ->
      case model.auth of
        Nothing ->
          (model, fetchToken env)
        Just auth ->
          withAllRequests (executeRequest (ApiAuth env.clientId auth)) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Lambda.Decrypt [clientId, clientSecret])

appendState : State -> Model -> (Model, Effect Msg)
appendState state model =
  { model | pendingRequests = List.append model.pendingRequests [state] }
    |> step

withAllRequests : (State -> Model -> (Model, Effect Msg)) -> Model -> (Model, Effect Msg)
withAllRequests f model =
  model.pendingRequests
    |> List.foldl (commandFold f) ({model | pendingRequests = []}, Lambda.NoEffect)

commandFold
  : (a -> model -> (model, Effect msg))
  -> a
  -> (model, Effect msg)
  -> (model, Effect msg)
commandFold f a (model, cmd) =
  let (m, c) = f a model in
  (m, Lambda.Batch [cmd, c])

executeRequest : ApiAuth -> State -> Model -> (Model, Effect Msg)
executeRequest auth state model =
  (model, stateRequest auth state)

stateRequest : ApiAuth -> State -> Effect Msg
stateRequest auth state =
  case state.request of
    FetchVideos {userId} ->
      fetchVideos auth userId state
    FetchVideosAndName {userId} ->
      fetchUserById auth userId state
    FetchVideosWithName user ->
      fetchVideosWithName auth user state
    FetchUser {userName} ->
      fetchUserByName auth userName state

errorResponse : Lambda.Session -> String -> Effect Msg
errorResponse session reason =
  Lambda.Response session (Err reason)

errorResponseState : String -> State -> model -> (model, Effect Msg)
errorResponseState reason state model =
  (model, errorResponse state.session reason)

sendResponse : Lambda.Session -> Value -> Effect Msg
sendResponse session response =
  Lambda.Response session (Ok response)

httpRequest : Http.Request Msg -> Effect Msg
httpRequest = Lambda.HttpRequest

standardHeaders =
  [ Port.header "User-Agent" "Schedule From Videos Lambda"
  ]

type alias ApiAuth =
  { clientId : Secret
  , token : Secret
  }

oauthHeaders : ApiAuth -> List Port.Header
oauthHeaders {clientId, token} =
  twitchHeaders (Secret.toString clientId) (Secret.toString token)
    |> List.append standardHeaders

twitchHeaders : String -> String -> List Port.Header
twitchHeaders clientId token =
  [ Port.header "Client-ID" clientId
  , Port.header "Authorization" ("Bearer "++token)
  ]

--tokenHostname = "localhost"
tokenHostname = "id.twitch.tv"

tokenPath : Env.PlainEnv -> String
tokenPath {clientId, clientSecret} =
  "/oauth2/token"
    ++ "?client_id=" ++ (Secret.toString clientId)
    ++ "&client_secret=" ++ (Secret.toString clientSecret)
    ++ "&grant_type=client_credentials"

fetchToken : Env.PlainEnv -> Effect Msg
fetchToken env =
  httpRequest
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , expect = Http.expectJson GotToken decodeToken
    }

decodeToken : Decode.Decoder Secret
decodeToken =
  Id.appOAuth
    |> Decode.map (.accessToken>>Secret.fromString)

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : ApiAuth -> String -> State -> Effect Msg
fetchVideos auth userId state =
  httpRequest
    { hostname = helixHostname
    , path = videosPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = Http.expectJson (httpResponse state "fetchVideos" State.GotVideos) decodeVideos
    }

fetchVideosWithName : ApiAuth -> {userId : String, userName: String} -> State -> Effect Msg
fetchVideosWithName auth user state =
  httpRequest
    { hostname = helixHostname
    , path = videosPath user.userId
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = Http.expectJson (httpResponse state "fetchVideos" (State.GotVideosWithName user)) decodeVideos
    }

decodeVideos : Decode.Decoder (List Helix.Video)
decodeVideos =
  Helix.videos
    |> Decode.map (List.filter (\v -> v.videoType == Helix.Archive))

fetchUserByIdPath : String -> String
fetchUserByIdPath userId =
  "/helix/users?id=" ++ userId

fetchUserById : ApiAuth -> String -> State -> Effect Msg
fetchUserById auth userId state =
  httpRequest
    { hostname = helixHostname
    , path =  fetchUserByIdPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = Http.expectJson (httpResponse state "fetchUserById" (validateUser State.GotUsersById)) Helix.users
    }

fetchUserByNamePath : String -> String
fetchUserByNamePath login =
  "/helix/users?login=" ++ login

fetchUserByName : ApiAuth -> String -> State -> Effect Msg
fetchUserByName auth login state =
  httpRequest
    { hostname = helixHostname
    , path =  fetchUserByNamePath login
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = Http.expectJson (httpResponse state "fetchUserByName" (validateUser State.GotUsersByName)) Helix.users
    }
