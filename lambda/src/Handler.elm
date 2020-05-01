module Handler exposing (main)

import Encode
import Env exposing (Env)
import Lambda
import Secret exposing (Secret)

import Twitch.Id.Decode as Id
import Twitch.Helix.Decode as Helix

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Platform


type alias Model =
  { env : Env
  , auth : Maybe Secret
  }

type Msg
  = Event (Result Decode.Error Lambda.EventState)

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
  }

type alias State = Decode.Value

newState : String -> Value -> State
newState userId session =
  Encode.object
    [ ("user_id", Encode.string userId)
    , ("session", session)
    ]

stateUserId : State -> Result Decode.Error String
stateUserId =
  Decode.decodeValue (Decode.field "user_id" Decode.string)

stateSession : State -> Result Decode.Error Value
stateSession =
  Decode.decodeValue (Decode.field "session" Decode.value)

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
      ( model
      , case Decode.decodeValue videosRequest data of
          Ok {userId} ->
            let state2 = newState userId state in
            case model.env of
              Env.Plain _ ->
                case model.auth of
                  Nothing ->
                    fetchToken model.env state2
                  Just auth ->
                    fetchVideos model.env auth userId state2
              Env.Encrypted {clientId, clientSecret} ->
                Lambda.decrypt [clientId, clientSecret] state2
          Err err ->
            let _ = Debug.log "event error" err in
            errorResponseSession state "unrecognized event"
      )
    Lambda.Decrypted (Ok [id, secret]) ->
      let
        env = Env.Plain
          { clientId = id
          , clientSecret = secret
          }
      in
      ( { model | env = env }
      , case model.auth of
        Nothing ->
          fetchToken env state
        Just auth ->
          case stateUserId state of
            Ok userId ->
              fetchVideos env auth userId state
            Err err ->
              let _ = Debug.log "event error" err in
              errorResponseState state "userid missing"
      )
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
      ( { model | auth = mauth }
      , case (mauth, stateUserId state) of
          (Just auth, Ok userId) ->
            fetchVideos model.env auth userId state
          (_, Err err) ->
            let _ = Debug.log "event error" err in
            errorResponseState state "userid missing"
          (Nothing, _) ->
            errorResponseState state "token decode error"
      )
    Lambda.HttpResponse "fetchVideos" (Ok json) ->
      let
        videos = json
          |> Decode.decodeValue Helix.videos
          |> Result.mapError (Debug.log "video decode error")
          |> Result.withDefault []
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> Encode.videos
          |> (\v -> Encode.object [ ("videos", v) ])
      in
        (model, sendResponse state videos)
    Lambda.HttpResponse tag (Ok json) ->
      let _ = Debug.log ("unknown response " ++ tag) json in
      (model, Cmd.none)
    Lambda.HttpResponse tag (Err err) ->
      let _ = Debug.log ("http error " ++ tag) err in
      (model, Cmd.none)

errorResponseSession : Value -> String -> Cmd Msg
errorResponseSession session reason =
  Lambda.response session (Err reason)

errorResponseState : Value -> String -> Cmd Msg
errorResponseState state reason =
  case stateSession state of
    Ok session -> errorResponseSession session reason
    Err err -> Debug.todo ("error response did not find session. " ++ reason)

sendResponse : Value -> Value -> Cmd Msg
sendResponse state response =
  case stateSession state of
    Ok session -> Lambda.response session (Ok response)
    Err err -> Debug.todo "response did not find session. "

standardHeaders =
  [ Lambda.header "User-Agent" "Schedule From Videos Lambda"
  ]

oauthHeaders : Env -> Secret -> List Lambda.Header
oauthHeaders env token =
  case env of
    Env.Encrypted _ ->
      Debug.todo "decrypt env"
    Env.Plain {clientId} ->
      twitchHeaders (Secret.toString clientId) (Secret.toString token)
        |> List.append standardHeaders

twitchHeaders : String -> String -> List Lambda.Header
twitchHeaders clientId token =
  [ Lambda.header "Client-ID" clientId
  , Lambda.header "Authorization" ("Bearer "++token)
  ]

--tokenHostname = "localhost"
tokenHostname = "id.twitch.tv"

tokenPath : Env -> String
tokenPath env =
  case env of
    Env.Encrypted _ ->
      Debug.todo "decrypt env"
    Env.Plain {clientId, clientSecret}->
      "/oauth2/token"
        ++ "?client_id=" ++ (Secret.toString clientId)
        ++ "&client_secret=" ++ (Secret.toString clientSecret)
        ++ "&grant_type=client_credentials"

fetchToken : Env -> State -> Cmd Msg
fetchToken env state =
  Lambda.httpRequest
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , tag = "fetchToken"
    }
    state

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : Env -> Secret -> String -> State -> Cmd Msg
fetchVideos env auth userId state =
  Lambda.httpRequest
    { hostname = helixHostname
    , path = videosPath userId
    , method = "GET"
    , headers = oauthHeaders env auth
    , tag = "fetchVideos"
    }
    state

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Event
