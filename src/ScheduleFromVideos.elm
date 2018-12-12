module ScheduleFromVideos exposing (..)

import MeasureText
import Twitch.Helix.Decode as Helix
import Twitch.Helix as Helix
import TwitchExt
import TwitchId
import ScheduleGraph exposing (Event)
import View exposing (Mode(..), Data(..))

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query
import Http
import Time exposing (Posix, Zone)
import Task

type Msg
  = UserForName (Result Http.Error (List Helix.User))
  | UserForId (Result Http.Error (List Helix.User))
  | Videos (Result Http.Error (List Helix.Video))
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | CurrentTime Posix
  | CurrentZone Zone
  | WindowSize (Int, Int)
  | TextSize MeasureText.TextSize
  | OnAuthorized TwitchExt.Auth
  | OnContext TwitchExt.Context
  | UI (View.Msg)

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , clientId : String
  , login : Data String
  , userId : Data String
  , events : Data (List Event)
  , time : Posix
  , zone : Zone
  , mode : Mode
  , theme : String
  , windowWidth : Int
  , windowHeight : Int
  , labelWidths : Dict String Float
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
  let
    mlogin = extractSearchArgument "login" location
    muserId = extractSearchArgument "userId" location
    manchor = extractSearchArgument "anchor" location
  in
  ( { location = location
    , navigationKey = key
    , clientId = TwitchId.clientId
    , login = mlogin |> Maybe.map Data |> Maybe.withDefault Unknown
    , userId = muserId |> Maybe.map Data |> Maybe.withDefault Unknown
    , events = Unknown
    , time = Time.millisToPosix 0
    , zone = Time.utc
    , mode = case manchor of
        Just _ -> Extension
        Nothing -> Page
    , theme = "dark"
    , windowWidth = 320
    , windowHeight = 300
    , labelWidths = Dict.empty
    }
  , Cmd.batch
    [ Task.perform CurrentTime Time.now
    , Task.perform CurrentZone Time.here
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    , ScheduleGraph.allDays
      |> List.map ScheduleGraph.dayName
      |> List.map (\name -> MeasureText.getTextWidth {font = "100px sans-serif", text = name})
      |> Cmd.batch
    , case muserId of
        Just id ->
          Cmd.batch
            [ fetchUserById TwitchId.clientId id
            , fetchVideos TwitchId.clientId id
            ]
        Nothing ->
          case mlogin of
            Just login -> fetchUserByName TwitchId.clientId login
            Nothing -> Cmd.none
    ]
  )

update msg model =
  case msg of
    UserForName (Ok (user::_)) ->
      ( { model
        | login = Data user.login
        , userId = Data user.id
        }
      , Cmd.batch
        [ fetchVideos model.clientId user.id
        , if (Data user.id) /= model.userId then
            Navigation.pushUrl model.navigationKey (model.location.path ++ "?userId="  ++ user.id)
          else
            Cmd.none
        ]
      )
    UserForName (Ok _) ->
      ({ model | login = NotFound, userId = Unknown }, Cmd.none)
    UserForName (Err error) ->
      ({ model | login = RequestFailed, userId = Unknown }, Cmd.none)
    UserForId (Ok (user::_)) ->
      ( { model
        | login = Data user.login
        , userId = Data user.id
        }
      , Cmd.none
      )
    UserForId (Ok _) ->
      ({ model | login = Unknown, userId = NotFound }, Cmd.none)
    UserForId (Err error) ->
      ({ model | login = Unknown, userId = RequestFailed }, Cmd.none)
    Videos (Ok videos) ->
      ( { model
        | events = videos
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> List.map (\v -> {start = v.createdAt, duration = v.duration})
          |> Data
        }
      , Cmd.none
      )
    Videos (Err (Http.BadStatus 503)) ->
      ({ model | events = NotFound }, Cmd.none)
    Videos (Err error) ->
      ({ model | events = RequestFailed }, Cmd.none)
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    CurrentTime time ->
      ( {model | time = time}, Cmd.none)
    CurrentZone zone ->
      ( {model | zone = zone}, Cmd.none)
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
    TextSize {text, width} ->
      ( {model | labelWidths = Dict.insert text (width/100) model.labelWidths}, Cmd.none)
    OnAuthorized auth ->
      case String.toInt auth.channelId of
        Just _ ->
          ( { model
            | clientId = auth.clientId
            , userId = Data auth.channelId
            }
          , fetchVideos auth.clientId auth.channelId
          )
        Nothing ->
          (model, Cmd.none)
    OnContext context ->
      ( { model | theme = context.theme }, Cmd.none )
    UI (View.SetUsername username) ->
      ( { model |events = Unknown }
      , fetchUserByName model.clientId username)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (60 * 1000) CurrentTime
    , Browser.Events.onResize (\w h -> WindowSize (w, h))
    , TwitchExt.onAuthorized OnAuthorized
    , TwitchExt.onContext OnContext
    , MeasureText.textSize TextSize
    ]

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> String -> Cmd Msg
fetchUserByName clientId login =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = UserForName
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> String -> Cmd Msg
fetchUserById clientId id =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = UserForId
    , url = (fetchUserByIdUrl id)
    }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : String -> String -> Cmd Msg
fetchVideos clientId userId =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Videos
    , url = (fetchVideosUrl userId)
    }

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
