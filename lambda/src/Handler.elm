module Handler exposing (main)

import Env exposing (Env)
import Lambda
import Secret exposing (Secret)

import Twitch.Id.Decode as Decode

import Json.Decode as Decode
import Platform


type alias Model =
  { env : Env
  , userId : Maybe String
  , auth : Maybe Secret
  }

type Msg
  = Event (Result Decode.Error Lambda.Event)

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
  , userId = Nothing
  , auth = Nothing
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Event (Ok (Lambda.Videos userid)) ->
      ( {model | userId = Just userid}
      , case model.auth of
        Nothing -> fetchToken model.env
        Just auth -> Debug.todo "reuse token"
      )
    Event (Ok (Lambda.Response "fetchToken" (Ok json))) ->
      ( { model | auth = json
          |> Decode.decodeValue Decode.appOAuth
          |> Result.map (.accessToken>>Secret.fromString)
          |> Result.mapError (Debug.log "token decode error")
          |> Result.toMaybe
        }
      , Cmd.none
      )
    Event (Ok (Lambda.Response tag (Ok json))) ->
      let _ = Debug.log tag json in
      (model, Cmd.none)
    Event (Ok (Lambda.Response tag (Err err))) ->
      let _ = Debug.log ("http error " ++ tag) err in
      (model, Cmd.none)
    Event (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)

standardHeaders =
  [ Lambda.header "User-Agent" "Schedule From Videos Lambda"
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

fetchToken : Env -> Cmd Msg
fetchToken env =
  Lambda.request
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , tag = "fetchToken"
    }

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Event
