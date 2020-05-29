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
  , pendingRequests : List State
  }

type Msg
  = NewEvent Value Lambda.Session
  | Decrypted (Result String (List Secret))
  | GotToken (Result HttpError Secret)
  | HttpError State String HttpError
  | GotVideos State (List Helix.Video)
  | GotUsersById State (List Helix.User)
  | GotUsersByName State (List Helix.User)

type HttpError
  = BadStatus Int String
  | NetworkError
  | BadBody Decode.Error

main = lambdaProgram
  { init = appInit
  , update = appUpdate
  }

appInit : Value -> (Model, Effect)
appInit flags =
  case (Env.decode flags) of
    Ok env -> (initialModel env, NoEffect)
    Err err -> Debug.todo ("env decode error" ++ (Debug.toString err))

initialModel : Env -> Model
initialModel env =
  { env = env
  , auth = Nothing
  , pendingRequests = []
  }

appUpdate : Msg -> Model -> (Model, Effect)
appUpdate msg model =
  case msg of
    NewEvent data session ->
      case Decode.decodeValue Event.event data of
        Ok ev ->
          appendState (stateForEvent ev session) model
        Err err ->
          let _ = Debug.log "event error" err in
          (model, errorResponse "unrecognized event" session)
    Decrypted (Ok [id, secret]) ->
      { model
      | env = Env.Plain
        { clientId = id
        , clientSecret = secret
        }
      }
        |> step
    Decrypted (Ok list) ->
      let _ = Debug.log ("decrypt wrong number of arguments") (List.length list) in
      withAllRequests (errorResponseState "service misconfiguration") model
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

stateForEvent : Event.Event -> Lambda.Session -> State
stateForEvent event session =
  case event of
    Event.Videos {userId} ->
      State.fetchVideos userId session
    Event.VideosWithName {userId} ->
      State.fetchVideosWithName userId session
    Event.User {userName} ->
      State.fetchUser userName session

step : Model -> (Model, Effect)
step model =
  case model.env of
    Env.Plain env ->
      case model.auth of
        Nothing ->
          (model, fetchToken env)
        Just auth ->
          withAllRequests (executeRequest (ApiAuth env.clientId auth)) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Decrypt [clientId, clientSecret])

appendState : State -> Model -> (Model, Effect)
appendState state model =
  { model | pendingRequests = List.append model.pendingRequests [state] }
    |> step

withAllRequests : (State -> Model -> (Model, Effect)) -> Model -> (Model, Effect)
withAllRequests f model =
  model.pendingRequests
    |> List.foldl (commandFold f) ({model | pendingRequests = []}, NoEffect)

commandFold
  : (a -> model -> (model, Effect))
  -> a
  -> (model, Effect)
  -> (model, Effect)
commandFold f a (model, cmd) =
  let (m, c) = f a model in
  (m, Batch [cmd, c])

executeRequest : ApiAuth -> State -> Model -> (Model, Effect)
executeRequest auth state model =
  (model, stateRequest auth state)

stateRequest : ApiAuth -> State -> Effect
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

errorResponse : String -> Lambda.Session -> Effect
errorResponse reason session =
  Response session (Err reason)

errorResponseState : String -> State -> model -> (model, Effect)
errorResponseState reason state model =
  (model, errorResponse reason state.session)

sendResponse : Lambda.Session -> Value -> Effect
sendResponse session response =
  Response session (Ok response)

type alias HttpRequest =
  { hostname : String
  , path : String
  , method : String
  , headers : List Lambda.Header
  , expect : Expect Msg
  }

httpRequest : HttpRequest -> Effect
httpRequest = Http

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

fetchToken : Env.PlainEnv -> Effect
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

fetchVideos : ApiAuth -> String -> State -> Effect
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

fetchUserById : ApiAuth -> String -> State -> Effect
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

fetchUserByName : ApiAuth -> String -> State -> Effect
fetchUserByName auth login state =
  httpRequest
    { hostname = helixHostname
    , path =  fetchUserByNamePath login
    , method = "GET"
    , headers = oauthHeaders auth
    , expect = expectJson (httpResponse state "fetchUserByName" GotUsersByName) Helix.users
    }


-------------------------------------

type alias HttpModel model =
  { model
  | nextRequestId : RequestId
  , outstandingRequests : Dict Int (Expect Msg)
  }

type alias LambdaModel model =
  { nextRequestId : RequestId
  , outstandingRequests : Dict Int (Expect Msg)
  , app : model
  }

type alias RequestId = Int

type Effect
  = NoEffect
  | Batch (List Effect)
  | Decrypt (List Secret)
  | Http HttpRequest
  | Response Lambda.Session (Result String Value)

perform : (HttpModel m, Effect) -> (HttpModel m, Cmd msg)
perform (model, effect) =
  case effect of
    NoEffect -> (model, Cmd.none)
    Batch effects ->
      List.foldl (\eff (m, cmd) ->
        let (m2, c2) = perform (m, eff) in
        (m2, Cmd.batch [cmd, c2])
      ) (model, Cmd.none) effects
    Decrypt secrets -> (model, Lambda.decrypt secrets)
    Http request -> rememberHttpRequest request model
    Response session result -> (model, Lambda.response session result)

type alias LambdaMsg = Lambda.Event
handle = identity

lambdaProgram :
  { init : Value -> (model, Effect)
  , update : Msg -> model -> (model, Effect)
  }
  -> Program Value (LambdaModel model) LambdaMsg
lambdaProgram {init, update} =
  Platform.worker
    { init = lambdaInit init
    , update = lambdaUpdate update
    , subscriptions = subscriptions
    }

lambdaInit : (flags -> (model, Effect)) -> flags -> (LambdaModel model, Cmd msg)
lambdaInit init flags =
  let (app, effect) = init flags in
  (initialLambdaModel app, Cmd.none)

initialLambdaModel : model -> LambdaModel model
initialLambdaModel app =
  { nextRequestId = 0
  , outstandingRequests = Dict.empty
  , app = app
  }

lambdaUpdate : (Msg -> model -> (model, Effect)) -> LambdaMsg -> LambdaModel model -> (LambdaModel model, Cmd LambdaMsg)
lambdaUpdate update event model =
  updateEvent update event model
    |> perform

updateEvent : (Msg -> model -> (model, Effect)) -> Lambda.Event -> LambdaModel model -> (LambdaModel model, Effect)
updateEvent update event model =
  case event of
    Lambda.SystemError err ->
      (model, NoEffect)
    Lambda.NewEvent data session ->
      let (app, effect) = update (NewEvent data session) model.app in
                                                                          ({model | app = app}, effect)
    Lambda.Decrypted result ->
      let (app, effect) = update (Decrypted result) model.app in
      ({model | app = app}, effect)
    Lambda.HttpResponse id result ->
      let
        (expect, m2) = httpMatch id model
        (app, effect) = update (decodeResponse expect (Result.mapError myError result)) model.app
      in
        ({m2 | app = app}, effect)


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

httpMatch : Int -> HttpModel m -> (Expect Msg, HttpModel m)
httpMatch id model =
  case Dict.get id model.outstandingRequests of
    Just expect ->
      (expect, { model | outstandingRequests = Dict.remove id model.outstandingRequests })
    Nothing ->
      Debug.todo "response to unknown request"

rememberHttpRequest : HttpRequest -> HttpModel m -> (HttpModel m, Cmd msg)
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

toLambdaRequest : RequestId -> HttpRequest -> Cmd msg
toLambdaRequest id req =
  Lambda.httpRequest
    { hostname = req.hostname
    , path = req.path
    , method = req.method
    , headers = req.headers
    , id = id
    }

subscriptions : LambdaModel m -> Sub LambdaMsg
subscriptions model = Lambda.event handle
