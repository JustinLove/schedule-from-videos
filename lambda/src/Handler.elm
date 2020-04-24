module Handler exposing (main)

import Env
import Lambda
import Secret

import Json.Decode as Decode
import Platform

type alias Model =
  { env : Env.Env
  , userId : Maybe String
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
    Ok env -> ({env = env, userId = Nothing }, Lambda.test)
    Err err -> Debug.todo ("env decode error" ++ (Debug.toString err))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Event (Ok (Lambda.Videos userid)) ->
      ({model | userId = Just userid}, Cmd.none)
    Event (Err err) ->
      let _ = Debug.log "error" err in
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Lambda.event Event
