module Handler exposing (main)

import Encode
import Env exposing (Env)
import Lambda
import Secret exposing (Secret)

import Twitch.Id.Decode as Id
import Twitch.Helix.Decode as Helix

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
      let
        auth = json
          |> Decode.decodeValue Id.appOAuth
          |> Result.map (.accessToken>>Secret.fromString)
          |> Result.mapError (Debug.log "token decode error")
          |> Result.toMaybe
      in
      ( { model | auth = auth }
      , fetchVideos model.env auth model.userId
      )
    Event (Ok (Lambda.Response "fetchVideos" (Ok json))) ->
      let
        videos = json
          |> Decode.decodeValue Helix.videos
          |> Result.mapError (Debug.log "video decode error")
          |> Result.withDefault []
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> Encode.videos
          |> Debug.log "videos"
      in
        (model, Cmd.none)
    Event (Ok (Lambda.Response tag (Ok json))) ->
      let _ = Debug.log ("unknown response " ++ tag) json in
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

oauthHeaders : Env -> Maybe Secret -> List Lambda.Header
oauthHeaders env auth =
  case (env, auth) of
    (Env.Encrypted _, _) ->
      Debug.todo "decrypt env"
    (_, Nothing) ->
      Debug.todo "not authenticated"
    (Env.Plain {clientId}, Just token) ->
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

fetchToken : Env -> Cmd Msg
fetchToken env =
  Lambda.request
    { hostname = tokenHostname
    , path = tokenPath env
    , method = "POST"
    , headers = standardHeaders
    , tag = "fetchToken"
    }

helixHostname = "api.twitch.tv"

videosPath : String -> String
videosPath userId =
  "/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : Env -> Maybe Secret -> Maybe String -> Cmd Msg
fetchVideos env auth muserId=
  case muserId of
    Nothing -> Cmd.none
    Just userId ->
      Lambda.request
        { hostname = helixHostname
        , path = Debug.log "path" <| videosPath userId
        , method = "GET"
        , headers = oauthHeaders env auth
        , tag = "fetchVideos"
        }

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Event
