module Handler exposing (main)

import Env exposing (Env)
import Event.Decode as Event
import Lambda exposing (Effect)
import Lambda.Port as Port
import Lambda.Http as Http
import Reply.Encode as Encode
import Secret exposing (Secret)
import State exposing (Retry(..), Request(..), State)

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
      perform (State.init data session) model
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
    WithState state stateMsg ->
      perform (State.update stateMsg state) model

perform : State.Effect -> Model -> (Model, Effect Msg)
perform effect model =
  case effect of
    State.Query newState ->
      model |> appendState newState
    State.AuthReset newState ->
      {model|auth = Nothing} |> appendState newState
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
  ( model
  , State.toHttp state.request
    |> withAuth auth
    |> Http.map (WithState state)
    |> Lambda.HttpRequest
  )

errorResponse : Lambda.Session -> String -> Effect Msg
errorResponse session reason =
  Lambda.Response session (Err reason)

errorResponseState : String -> State -> model -> (model, Effect Msg)
errorResponseState reason state model =
  (model, errorResponse state.session reason)

sendResponse : Lambda.Session -> Value -> Effect Msg
sendResponse session response =
  Lambda.Response session (Ok response)

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

withAuth : ApiAuth -> Http.Request msg -> Http.Request msg
withAuth auth request =
  { request | headers = oauthHeaders auth }

tokenUrl : Env.PlainEnv -> String
tokenUrl {clientId, clientSecret} =
  "https://id.twitch.tv/oauth2/token"
    ++ "?client_id=" ++ (Secret.toString clientId)
    ++ "&client_secret=" ++ (Secret.toString clientSecret)
    ++ "&grant_type=client_credentials"

fetchToken : Env.PlainEnv -> Effect Msg
fetchToken env =
  Lambda.HttpRequest
    { url = tokenUrl env
    , method = "POST"
    , headers = standardHeaders
    , expect = Http.expectJson GotToken decodeToken
    }

decodeToken : Decode.Decoder Secret
decodeToken =
  Id.appOAuth
    |> Decode.map (.accessToken>>Secret.fromString)

