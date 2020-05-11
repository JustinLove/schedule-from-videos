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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Handle (Ok (Lambda.EventState state event)) ->
      updateEvent event state model
    Handle (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)

updateEvent : Lambda.Event -> Value -> Model -> (Model, Cmd Msg)
updateEvent event stateValue model =
  case event of
    Lambda.NewEvent data ->
      case Decode.decodeValue Event.event data of
        Ok (Event.Videos {userId}) ->
          { model | pendingRequests = List.append
            model.pendingRequests
            [State.fetchVideos userId stateValue]
          }
            |> step
        Ok (Event.VideosWithName {userId}) ->
          { model | pendingRequests = List.append
            model.pendingRequests
            [State.fetchVideosWithName userId stateValue]
          }
            |> step
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
      { model | env = env }
        |> step
    Lambda.Decrypted (Ok _) ->
      let _ = Debug.log ("decrypt wrong number of results ") in
      withAllRequests (errorResponseState "service misconfiguration") model
    Lambda.Decrypted (Err err) ->
      let _ = Debug.log ("decrypt error ") err in
      withAllRequests (errorResponseState "service misconfiguration") model
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
        videos = json
          |> Decode.decodeValue Helix.videos
          |> Result.mapError (Debug.log "video decode error")
          |> Result.withDefault []
          |> List.filter (\v -> v.videoType == Helix.Archive)
      in
        case Decode.decodeState stateValue of
          Ok state ->
            case state.request of
              FetchVideos _ ->
                ( model
                , Encode.object
                  [ ("events", Encode.videos videos)
                  ]
                  |> sendResponse state.session
                )
              FetchVideosWithName {userId, userName} ->
                ( model
                , Encode.object
                  [ ("user", Encode.user userId userName)
                  , ("events", Encode.videos videos)
                  ]
                  |> sendResponse state.session
                )
              _ ->
                Debug.todo "videos response in user fetch state"
          Err err ->
            Debug.todo "unparsable state"
    Lambda.HttpResponse "fetchUserById" (Ok json) ->
      let
        muser = json
          |> Decode.decodeValue Helix.users
          |> Result.mapError (Debug.log "user decode error")
          |> Result.withDefault []
          |> List.head
      in
        case Decode.decodeState stateValue of
          Ok state ->
            case muser of
              Just user ->
                { model
                | pendingRequests = List.append
                    model.pendingRequests
                    [{state|request = FetchVideosWithName
                      { userId = user.id
                      , userName = user.displayName
                      }
                    }]
                }
                  |> step
              Nothing ->
                (model, errorResponse "user not found" state.session)
          Err err ->
            Debug.todo "unparsable state"
    Lambda.HttpResponse tag (Ok json) ->
      let _ = Debug.log ("unknown response " ++ tag) json in
      (model, Cmd.none)
    Lambda.HttpResponse tag (Err (Lambda.BadStatus 401 body)) ->
      let _ = Debug.log ("auth failed " ++ tag) body in
      case Decode.decodeState stateValue of
        Ok s ->
          if s.shouldRetry == WillRetry then
            { model
            | auth = Nothing
            , pendingRequests = List.append
                model.pendingRequests
                [{s|shouldRetry = Retried}]
            }
              |> step
          else
            (model, errorResponse "unable to authenticate" s.session)
        Err err ->
          Debug.todo "401 on request with non-request state"
    Lambda.HttpResponse "fetchToken" (Err err) ->
      let _ = Debug.log "unable to fetch token" err in
      withAllRequests (errorResponseState "unable to fetch token") model
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
          withAllRequests (executeRequest (ApiAuth env.clientId auth)) model
    Env.Encrypted {clientId, clientSecret} ->
      (model, Lambda.decrypt [clientId, clientSecret])

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

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=1&type=archive&user_id=" ++ userId

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

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Handle
