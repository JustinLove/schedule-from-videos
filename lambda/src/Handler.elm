module Handler exposing (main)

import Env exposing (Env)
import Lambda
import Reply.Encode as Encode
import Secret exposing (Secret)
import State exposing (Retry(..), FetchVideos, State)
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
  , pendingRequests : List Request
  }

type Msg
  = Event (Result Decode.Error Lambda.EventState)

type Request
  = FetchVideosRequest FetchVideos

main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

init : Decode.Value -> (Model, Cmd msg)
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

type alias VideosRequest =
  { userId : String }

videosRequest : Decode.Decoder VideosRequest
videosRequest =
  Decode.map VideosRequest (Decode.field "user_id" Decode.string)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Event (Ok (Lambda.EventState state event)) ->
      updateEvent event state model
    Event (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)

updateEvent : Lambda.Event -> State -> Model -> (Model, Cmd Msg)
updateEvent event state model =
  case event of
    Lambda.NewEvent data ->
      case Decode.decodeValue videosRequest data of
        Ok {userId} ->
          { model | pendingRequests = List.append
            [FetchVideosRequest (State.fetchVideos userId state)]
            model.pendingRequests
          }
            |> step
        Err err ->
          let _ = Debug.log "event error" err in
          (model, errorResponse state "unrecognized event")
    Lambda.Decrypted (Ok [id, secret]) ->
      let
        env = Env.Plain
          { clientId = id
          , clientSecret = secret
          }
      in
      { model | env = env }
        |> step
    Lambda.Decrypted (Ok _) ->
      let _ = Debug.log ("decrypt wrong number of results ") in
      (model, Cmd.none)
    Lambda.Decrypted (Err err) ->
      let _ = Debug.log ("decrypt error ") err in
      (model, Cmd.none)
    Lambda.HttpResponse "fetchToken" (Ok json) ->
      let
        mauth = json
          |> Decode.decodeValue Id.appOAuth
          |> Result.map (.accessToken>>Secret.fromString)
          |> Result.mapError (Debug.log "token decode error")
          |> Result.toMaybe
      in
      { model | auth = mauth }
        |> step
    Lambda.HttpResponse "fetchVideos" (Ok json) ->
      let
        session = state
          |> Decode.fetchVideos
          |> Result.map .session
        videos = json
          |> Decode.decodeValue Helix.videos
          |> Result.mapError (Debug.log "video decode error")
          |> Result.withDefault []
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> Encode.videos
          |> (\v -> Encode.object [ ("videos", v) ])
      in
        (model, sendResponse session videos)
    Lambda.HttpResponse tag (Ok json) ->
      let _ = Debug.log ("unknown response " ++ tag) json in
      (model, Cmd.none)
    Lambda.HttpResponse tag (Err (Lambda.BadStatus 401 body)) ->
      let _ = Debug.log ("auth failed" ++ tag) body in
      --let state2 = retried state in
      (model, Cmd.none)
    Lambda.HttpResponse tag (Err err) ->
      let _ = Debug.log ("http error " ++ tag) err in
      (model, Cmd.none)

step : Model -> (Model, Cmd Msg)
step model =
  case model.env of
    Env.Plain env ->
      case model.auth of
        Nothing ->
          (model, fetchToken env)
        Just auth ->
          executeNextRequest (ApiAuth env.clientId auth) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Lambda.decrypt [clientId, clientSecret])

executeNextRequest : ApiAuth -> Model -> (Model, Cmd Msg)
executeNextRequest auth model = 
  case model.pendingRequests of
    next :: rest ->
      ({model | pendingRequests = rest}, executeRequest auth next)
    [] ->
      (model, Cmd.none)

executeRequest : ApiAuth -> Request -> Cmd Msg
executeRequest auth request = 
  case request of
    FetchVideosRequest state ->
      fetchVideos auth state.userId state

errorResponse : Value -> String -> Cmd Msg
errorResponse session reason =
  Lambda.response session (Err reason)

sendResponse : Result Decode.Error Value -> Value -> Cmd Msg
sendResponse rsession response =
  case rsession of
    Ok session -> Lambda.response session (Ok response)
    Err err -> Debug.todo "response did not find session. "

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

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : ApiAuth -> String -> FetchVideos -> Cmd Msg
fetchVideos auth userId state =
  Lambda.httpRequest
    { hostname = helixHostname
    , path = videosPath userId
    , method = "GET"
    , headers = oauthHeaders auth
    , tag = "fetchVideos"
    }
    (Encode.fetchVideos state)

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Event
