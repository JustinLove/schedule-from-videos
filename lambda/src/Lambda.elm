module Lambda exposing
  ( Session
  , Effect(..)
  , program
  )

import Lambda.Port as Port
import Lambda.Http as Http

import Secret exposing (Secret)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value)

type alias Session = Port.Session

type alias Model model msg =
  { nextRequestId : Http.RequestId
  , outstandingRequests : Dict Int (Http.Expect msg)
  , app : model
  }

initialModel : model -> Model model msg
initialModel app =
  { nextRequestId = 0
  , outstandingRequests = Dict.empty
  , app = app
  }

type Effect msg
  = NoEffect
  | Batch (List (Effect msg))
  | Decrypt (List Secret)
  | Http (Http.Request msg)
  | Response Session (Result String Value)

perform : (Model model appMsg, Effect appMsg) -> (Model model appMsg, Cmd msg)
perform (model, effect) =
  case effect of
    NoEffect -> (model, Cmd.none)
    Batch effects ->
      List.foldl (\eff (m, cmd) ->
        let (m2, c2) = perform (m, eff) in
        (m2, Cmd.batch [cmd, c2])
      ) (model, Cmd.none) effects
    Decrypt secrets -> (model, Port.decrypt secrets)
    Http request -> Http.rememberHttpRequest request model
    Response session result -> (model, Port.response session result)

program :
  { init : flags -> (model, Effect msg)
  , update : msg -> model -> (model, Effect msg)
  , newEvent : Value -> Port.Session -> msg
  , decrypted : (Result String (List Secret)) -> msg
  }
  -> Program flags (Model model msg) LambdaMsg
program config =
  Platform.worker
    { init = init config.init
    , update = (\event model -> update config.newEvent config.decrypted config.update event model |> perform)
    , subscriptions = subscriptions
    }

init : (flags -> (model, Effect appMsg)) -> flags -> (Model model appMsg, Cmd msg)
init appInit flags =
  let (app, effect) = appInit flags in
  (initialModel app, Cmd.none)

type alias LambdaMsg = Port.Event
handle = identity

update
  : (Value -> Port.Session -> msg)
  -> ((Result String (List Secret)) -> msg)
  -> (msg -> model -> (model, Effect msg))
  -> LambdaMsg
  -> Model model msg
  -> (Model model msg, Effect msg)
update newEvent decrypted appUpdate event model =
  case event of
    Port.SystemError err ->
      (model, NoEffect)
    Port.NewEvent data session ->
      let (app, effect) = appUpdate (newEvent data session) model.app in
                                                                          ({model | app = app}, effect)
    Port.Decrypted result ->
      let (app, effect) = appUpdate (decrypted result) model.app in
      ({model | app = app}, effect)
    Port.HttpResponse id result ->
      let
        (expect, m2) = Http.httpMatch id model
        (app, effect) = appUpdate (Http.decodeResponse expect (Result.mapError Http.publicError result)) model.app
      in
        ({m2 | app = app}, effect)

subscriptions : Model model msg -> Sub LambdaMsg
subscriptions model = Port.event handle
