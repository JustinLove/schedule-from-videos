module Lambda exposing (main)

import Env

import Json.Decode
import Platform

type alias Model = Env.Env
type alias Msg = ()

main = Platform.worker
  { init = init
  , update = update
  , subscriptions = \model -> Sub.none
  }

init : Json.Decode.Value -> (Model, Cmd msg)
init flags =
  case (Env.decode flags) of
    Ok env -> (env, Cmd.none)
    Err err -> Debug.todo ("env decode error" ++ (Debug.toString err))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
