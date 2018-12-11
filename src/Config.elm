module Config exposing (..)

import Config.View as View
import TwitchExt
import TwitchId

import Browser
import Browser.Navigation as Navigation
import Url exposing (Url)

type Msg
  = CurrentUrl Url
  | Navigate Browser.UrlRequest
  | OnAuthorized TwitchExt.Auth
  | OnContext TwitchExt.Context
  | UI (View.Msg)

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , clientId : String
  , userId : Maybe String
  , theme : String
  }

main = Browser.application
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  ( { location = location
    , navigationKey = key
    , clientId = TwitchId.clientId
    , userId = Nothing
    , theme = "dark"
    }
  , Cmd.none
  )

update msg model =
  case msg of
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    OnAuthorized auth ->
      case String.toInt auth.channelId of
        Just _ ->
          ( { model
            | clientId = auth.clientId
            , userId = Just auth.channelId
            }
          , Cmd.none
          )
        Nothing ->
          (model, Cmd.none)
    OnContext context ->
      ( { model | theme = context.theme }, Cmd.none )
    UI _ ->
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ TwitchExt.onAuthorized OnAuthorized
    , TwitchExt.onContext OnContext
    ]

