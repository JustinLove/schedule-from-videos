module Handler exposing (main)

import Env exposing (Env)
import Event.Decode as Event
import Lambda
import Reply.Encode as Encode
import Secret exposing (Secret)
import State exposing (Retry(..), Request(..), State)
--import State.Decode as Decode
--import State.Encode as Encode

import Twitch.Id.Decode as Id
import Twitch.Helix.Decode as Helix

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Platform

type alias Model =
  { env : Env
  , auth : Maybe Secret
  , nextRequestId : RequestId
  , outstandingRequests : Dict Int (Expect Msg)
  , pendingRequests : List State
  }

type alias RequestId = Int

type Msg
  = Handle Lambda.Event
  | NewEvent Value Value
  | Decrypted (Result String Env)
  | GotToken (Result HttpError Secret)
  | HttpError State String HttpError
  | GotVideos State (List Helix.Video)
  | GotUsersById State (List Helix.User)
  | GotUsersByName State (List Helix.User)

type HttpError
  = BadStatus Int String
  | NetworkError
  | BadBody Decode.Error
  | UnknownResponse

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
  , nextRequestId = 0
  , outstandingRequests = Dict.empty
  , pendingRequests = []
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Handle event ->
      updateEvent event model
    NewEvent data session ->
      case Decode.decodeValue Event.event data of
        Ok ev ->
          appendState (stateForEvent ev session) model
        Err err ->
          let _ = Debug.log "event error" err in
          (model, errorResponse "unrecognized event" session)
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
    HttpError state source (BadStatus 401 body) ->
      let _ = Debug.log ("auth failed " ++ source) body in
      if state.shouldRetry == WillRetry then
        { model | auth = Nothing }
          |> appendState {state|shouldRetry = Retried}
      else
        (model, errorResponse "unable to authenticate" state.session)
    HttpError state source (error) ->
      let _ = Debug.log ("http error: " ++ source) error in
      (model, errorResponse "service http error" state.session)
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

updateEvent : Lambda.Event -> Model -> (Model, Cmd Msg)
updateEvent event model =
  case event of
    Lambda.SystemError err ->
      (model, Cmd.none)
    Lambda.NewEvent data session ->
      update (NewEvent data session) model
    Lambda.Decrypted result ->
      result
        |> Result.andThen (\list ->
          case list of
            [id, secret] ->
              Ok <| Env.Plain
                { clientId = id
                , clientSecret = secret
                }
            _ ->
              Err "decrypt wrong number of results "
          )
        |> Decrypted
        |> (\msg -> update msg model)
    Lambda.HttpResponse id result ->
      let (expect, m2) = httpMatch id model in
      update (decodeResponse expect (Result.mapError myError result)) m2

myError : Lambda.HttpError -> HttpError
myError error =
  case error of
    Lambda.BadStatus status body -> BadStatus status body
    Lambda.NetworkError -> NetworkError

httpResponse : State -> String -> (State -> a -> Msg) -> Result HttpError a -> Msg
httpResponse state source success result =
  case result of
    Ok value -> success state value
    Err err -> HttpError state source err

decodeResponse : Expect msg -> Result HttpError String -> msg
decodeResponse expect response =
  case expect of
    ExpectString decodeTagger ->
      decodeTagger response

type Expect msg
  = ExpectString (Result HttpError String -> msg)

expectJson : (Result HttpError a -> msg) -> Decode.Decoder a -> Expect msg
expectJson tagger decoder =
  ExpectString (Result.andThen (Decode.decodeString decoder >> Result.mapError BadBody)
    >> tagger
  )

httpMatch : Int -> Model -> (Expect Msg, Model)
httpMatch id model =
  case Dict.get id model.outstandingRequests of
    Just expect ->
      (expect, { model | outstandingRequests = Dict.remove id model.outstandingRequests })
    Nothing ->
      Debug.todo "response to unknown request"

stateForEvent : Event.Event -> Lambda.Session -> State
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
          rememberHttpRequest (fetchToken env) model
        Just auth ->
          withAllRequests (executeRequest (ApiAuth env.clientId auth)) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Lambda.decrypt [clientId, clientSecret])

appendState : State -> Model -> (Model, Cmd Msg)
appendState state model =
  { model | pendingRequests = List.append model.pendingRequests [state] }
    |> step

withAllRequests : (State -> Model -> (Model, Cmd msg)) -> Model -> (Model, Cmd msg)
withAllRequests f model =
  model.pendingRequests
    |> List.foldl (commandFold f) ({model | pendingRequests = []}, Cmd.none)

commandFold
  : (a -> Model -> (Model, Cmd msg))
  -> a
  -> (Model, Cmd msg)
  -> (Model, Cmd msg)
commandFold f a (model, cmd) =
  let (m, c) = f a model in
  (m, Cmd.batch [cmd, c])

executeRequest : ApiAuth -> State -> Model -> (Model, Cmd Msg)
executeRequest auth state model =
  rememberHttpRequest (stateRequest auth state) model

stateRequest : ApiAuth -> State -> HttpRequest
stateRequest auth state =
  case state.request of
    FetchVideos {userId} ->
      fetchVideos auth userId state
    FetchVideosAndName {userId} ->
      fetchUserById auth userId state
    FetchVideosWithName {userId} ->
      fetchVideos auth userId state
    FetchUser {userName} ->
      fetchUserByName auth userName state

rememberHttpRequest : HttpRequest -> Model -> (Model, Cmd Msg)
rememberHttpRequest req model =
  let
    id = model.nextRequestId + 1
  in
  ( { model
    | nextRequestId = id
    , outstandingRequests = Dict.insert id req.expect model.outstandingRequests
    }
  , toLambdaRequest id req
  )

errorResponse : String -> Lambda.Session -> Cmd msg
errorResponse reason session =
  Lambda.response session (Err reason)

errorResponseState : String -> State -> Model -> (Model, Cmd msg)
errorResponseState reason state model =
  (model, errorResponse reason state.session)

sendResponse : Lambda.Session -> Value -> Cmd msg
sendResponse session response =
  Lambda.response session (Ok response)

type alias HttpRequest =
  { hostname : String
  , path : String
  , method : String
  , headers : List Lambda.Header
  , expect : Expect Msg
  }

httpRequest : HttpRequest -> HttpRequest
httpRequest = identity

toLambdaRequest : RequestId -> HttpRequest -> Cmd msg
toLambdaRequest id req =
  Lambda.httpRequest
    { hostname = req.hostname
    , path = req.path
    , method = req.method
    , headers = req.headers
    , id = id
    }

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

fetchToken : Env.PlainEnv -> HttpRequest
fetchToken env =
  httpRequest
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , expect = expectJson GotToken decodeToken
    }

decodeToken : Decode.Decoder Secret
decodeToken =
  Id.appOAuth
    |> Decode.map (.accessToken>>Secret.fromString)

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : ApiAuth -> String -> State -> HttpRequest
fetchVideos auth userId state =
  httpRequest
    { hostname = helixHostname
    , path = videosPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = expectJson (httpResponse state "fetchVideos" GotVideos) decodeVideos
    }

decodeVideos : Decode.Decoder (List Helix.Video)
decodeVideos =
  Helix.videos
    |> Decode.map (List.filter (\v -> v.videoType == Helix.Archive))

fetchUserByIdPath : String -> String
fetchUserByIdPath userId =
  "/helix/users?id=" ++ userId

fetchUserById : ApiAuth -> String -> State -> HttpRequest
fetchUserById auth userId state =
  httpRequest
    { hostname = helixHostname
    , path =  fetchUserByIdPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = expectJson (httpResponse state "fetchUserById" GotUsersById) Helix.users
    }

fetchUserByNamePath : String -> String
fetchUserByNamePath login =
  "/helix/users?login=" ++ login

fetchUserByName : ApiAuth -> String -> State -> HttpRequest
fetchUserByName auth login state =
  httpRequest
    { hostname = helixHostname
    , path =  fetchUserByNamePath login
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = expectJson (httpResponse state "fetchUserByName" GotUsersByName) Helix.users
    }

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Handle
