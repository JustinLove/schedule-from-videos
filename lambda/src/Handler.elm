module Handler exposing (main)

import Env exposing (Env)
import Event.Decode as Event
import Lambda
import Reply.Encode as Encode
import Secret exposing (Secret)
import State exposing (Retry(..), Request(..), State)
import State.Decode as Decode
import State.Encode as Encode

import Twitch.Id.Decode as Id
import Twitch.Helix.Decode as Helix

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Platform

type alias Model =
  { env : Env
  , auth : Maybe Secret
  , pendingRequests : List State
  }

type Msg
  = Handle (Result Decode.Error Lambda.EventState)
  | NewEvent State
  | Decrypted Env
  | GotToken (Result Lambda.HttpError (Maybe Secret))
  | HttpError State String HttpError
  | GotVideos State (List Helix.Video)
  | GotUsersById State (List Helix.User)
  | GotUsersByName State (List Helix.User)

type HttpError
  = BadStatus Int String
  | NetworkError
  | BadBody Decode.Error

main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

init : Value -> (Model, Cmd msg)
init flags =
  case (Env.decode flags) of
    Ok env -> (initialModel env, Cmd.none)
    Err err -> Debug.todo ("env decode error" ++ (Debug.toString err))

initialModel : Env -> Model
initialModel env =
  { env = env
  , auth = Nothing
  , pendingRequests = []
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Handle (Ok (Lambda.EventState state event)) ->
      updateEvent event state model
    Handle (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)
    NewEvent state ->
      appendState state model
    Decrypted env ->
      { model | env = env }
        |> step
    GotToken (Ok auth) ->
      { model | auth = auth }
        |> step
    GotToken (Err err) ->
      let _ = Debug.log "unable to fetch token" err in
      withAllRequests (errorResponseState "unable to fetch token") model
    HttpError state source (BadStatus 401 body) ->
      let _ = Debug.log ("auth failed " ++ source) body in
      if state.shouldRetry == WillRetry then
        { model | auth = Nothing }
          |> appendState {state|shouldRetry = Retried}
      else
        (model, errorResponse "unable to authenticate" state.session)
    HttpError state source (error) ->
      let _ = Debug.log ("http error: " ++ source) error in
      (model, Cmd.none)
    GotVideos state videos ->
      case state.request of
        FetchVideos _ ->
          ( model
          , Encode.videosReply {events = videos}
            |> sendResponse state.session
          )
        FetchVideosWithName {userId, userName} ->
          ( model
          , Encode.videosWithNameReply
            { user = { id = userId, name = userName }
            , events = videos
            }
            |> sendResponse state.session
          )
        _ ->
          Debug.todo "videos response in user fetch state"
    GotUsersById state users ->
      case users of
        user :: _ ->
          model
            |> appendState
              {state|request = FetchVideosWithName
                { userId = user.id
                , userName = user.displayName
                }
              }
        [] ->
          (model, errorResponse "user not found" state.session)
    GotUsersByName state users ->
      case users of
        user :: _ ->
          ( model
          , Encode.userReply { user = {id = user.id, name = user.displayName } }
            |> sendResponse state.session
          )
        [] ->
          (model, errorResponse "user not found" state.session)

updateEvent : Lambda.Event -> Value -> Model -> (Model, Cmd Msg)
updateEvent event stateValue model =
  case event of
    Lambda.NewEvent data ->
      case Decode.decodeValue Event.event data of
        Ok ev ->
          update (NewEvent (stateForEvent ev stateValue)) model
        Err err ->
          let _ = Debug.log "event error" err in
          (model, errorResponse "unrecognized event" stateValue)
    Lambda.Decrypted (Ok [id, secret]) ->
      let
        env = Env.Plain
          { clientId = id
          , clientSecret = secret
          }
      in
        update (Decrypted env) model
    Lambda.Decrypted (Ok _) ->
      let _ = Debug.log ("decrypt wrong number of results ") in
      withAllRequests (errorResponseState "service misconfiguration") model
    Lambda.Decrypted (Err err) ->
      let _ = Debug.log ("decrypt error ") err in
      withAllRequests (errorResponseState "service misconfiguration") model
    Lambda.HttpResponse "fetchToken" (Ok body) ->
      let
        mauth = body
          |> Decode.decodeString decodeToken
          |> Result.mapError (Debug.log "token decode error")
          |> Result.toMaybe
      in
        update (GotToken (Ok mauth)) model
    Lambda.HttpResponse "fetchToken" (Err err) ->
      update (GotToken (Err err)) model
    Lambda.HttpResponse tag (Ok body) ->
      case Decode.decodeState stateValue of
        Ok state ->
          case tag of
            "fetchVideos" ->
              update (decodeResponse (expectJson (httpResponse state tag GotVideos) decodeVideos) body) model
            "fetchUserById" ->
              update (decodeResponse (expectJson (httpResponse state tag GotUsersById) Helix.users) body) model
            "fetchUserByName" ->
              update (decodeResponse (expectJson (httpResponse state tag GotUsersByName) Helix.users) body) model
            _ ->
              let _ = Debug.log ("unknown response " ++ tag) body in
              (model, Cmd.none)
        Err err ->
          Debug.todo "unparsable state"
    Lambda.HttpResponse tag (Err err) ->
      case Decode.decodeState stateValue of
        Ok s ->
          update (HttpError s tag (myError err)) model
        Err _ ->
          Debug.todo "http error on request with non-request state"

myError : Lambda.HttpError -> HttpError
myError error =
  case error of
    Lambda.BadStatus status body -> BadStatus status body
    Lambda.NetworkError -> NetworkError

decodeResponse : Expect Msg -> String -> Msg
decodeResponse expect body =
  case expect of
    ExpectJson tagger ->
      tagger body

type Expect msg = ExpectJson (String -> msg)

expectJson : (Result HttpError a -> msg) -> Decode.Decoder a -> Expect msg
expectJson tagger decoder =
  ExpectJson (Decode.decodeString decoder
    >> Result.mapError BadBody
    >> tagger
  )

httpResponse : State -> String -> (State -> a -> Msg) -> Result HttpError a -> Msg
httpResponse state source success result =
  case result of
    Ok value -> success state value
    Err err -> HttpError state source err

stateForEvent : Event.Event -> Value -> State
stateForEvent event session =
  case event of
    Event.Videos {userId} ->
      State.fetchVideos userId session
    Event.VideosWithName {userId} ->
      State.fetchVideosWithName userId session
    Event.User {userName} ->
      State.fetchUser userName session

step : Model -> (Model, Cmd Msg)
step model =
  case model.env of
    Env.Plain env ->
      case model.auth of
        Nothing ->
          (model, fetchToken env)
        Just auth ->
          withAllRequests (executeRequest (ApiAuth env.clientId auth)) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Lambda.decrypt [clientId, clientSecret])

appendState : State -> Model -> (Model, Cmd Msg)
appendState state model =
  { model | pendingRequests = List.append model.pendingRequests [state] }
    |> step

withAllRequests : (State -> Cmd Msg) -> Model -> (Model, Cmd Msg)
withAllRequests f model =
  ({model | pendingRequests = []}
  , model.pendingRequests
    |> List.map f
    |> Cmd.batch
  )

executeRequest : ApiAuth -> State -> Cmd Msg
executeRequest auth state =
  case state.request of
    FetchVideos {userId} ->
      fetchVideos auth userId state
    FetchVideosAndName {userId} ->
      fetchUserById auth userId state
    FetchVideosWithName {userId} ->
      fetchVideos auth userId state
    FetchUser {userName} ->
      fetchUserByName auth userName state

errorResponse : String -> Value -> Cmd Msg
errorResponse reason session =
  Lambda.response session (Err reason)

errorResponseState : String -> State -> Cmd Msg
errorResponseState reason state =
  errorResponse reason state.session

sendResponse : Value -> Value -> Cmd Msg
sendResponse session response =
  Lambda.response session (Ok response)

standardHeaders =
  [ Lambda.header "User-Agent" "Schedule From Videos Lambda"
  ]

type alias ApiAuth =
  { clientId : Secret
  , token : Secret
  }

oauthHeaders : ApiAuth -> List Lambda.Header
oauthHeaders {clientId, token} =
  twitchHeaders (Secret.toString clientId) (Secret.toString token)
    |> List.append standardHeaders

twitchHeaders : String -> String -> List Lambda.Header
twitchHeaders clientId token =
  [ Lambda.header "Client-ID" clientId
  , Lambda.header "Authorization" ("Bearer "++token)
  ]

--tokenHostname = "localhost"
tokenHostname = "id.twitch.tv"

tokenPath : Env.PlainEnv -> String
tokenPath {clientId, clientSecret} =
  "/oauth2/token"
    ++ "?client_id=" ++ (Secret.toString clientId)
    ++ "&client_secret=" ++ (Secret.toString clientSecret)
    ++ "&grant_type=client_credentials"

fetchToken : Env.PlainEnv -> Cmd Msg
fetchToken env =
  Lambda.httpRequest
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , tag = "fetchToken"
    }
    Encode.null

decodeToken : Decode.Decoder Secret
decodeToken =
  Id.appOAuth
    |> Decode.map (.accessToken>>Secret.fromString)

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : ApiAuth -> String -> State -> Cmd Msg
fetchVideos auth userId state =
  Lambda.httpRequest
    { hostname = helixHostname
    , path = videosPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , tag = "fetchVideos"
    }
    (Encode.state state)

decodeVideos : Decode.Decoder (List Helix.Video)
decodeVideos =
  Helix.videos
    |> Decode.map (List.filter (\v -> v.videoType == Helix.Archive))

fetchUserByIdPath : String -> String
fetchUserByIdPath userId =
  "/helix/users?id=" ++ userId

fetchUserById : ApiAuth -> String -> State -> Cmd Msg
fetchUserById auth userId state =
  Lambda.httpRequest
    { hostname = helixHostname
    , path =  fetchUserByIdPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , tag = "fetchUserById"
    }
    (Encode.state state)

fetchUserByNamePath : String -> String
fetchUserByNamePath login =
  "/helix/users?login=" ++ login

fetchUserByName : ApiAuth -> String -> State -> Cmd Msg
fetchUserByName auth login state =
  Lambda.httpRequest
    { hostname = helixHostname
    , path =  fetchUserByNamePath login
    , method = "GET"
    , headers = oauthHeaders auth
    , tag = "fetchUserByName"
    }
    (Encode.state state)

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Handle
