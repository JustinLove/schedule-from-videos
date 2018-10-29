module ScheduleFromVideos exposing (..)

import MeasureText
import Twitch.Helix.Decode as Helix
import Twitch.Helix as Helix
import TwitchExt
import TwitchId
import ScheduleGraph exposing (Event)
import View exposing (Mode(..))

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

requestLimit = 100
rateLimit = 30
requestRate = 60*1000/rateLimit

type Msg
  = User (Result Http.Error (List Helix.User))
  | Videos (Result Http.Error (List Helix.Video))
  | Response Msg
  | NextRequest Posix
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
  , login : Maybe String
  , userId : Maybe String
  , events : List Event
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
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
    mlogin = Debug.log "Login" <| extractSearchArgument "login" location
    muserId = Debug.log "userId" <| extractSearchArgument "userId" location
    manchor = Debug.log "anchor" <| extractSearchArgument "anchor" location
  in
  ( { location = location
    , navigationKey = key
    , login = mlogin
    , userId = muserId
    , events = []
    , pendingRequests = [
      case muserId of
        Just id -> fetchUserById id
        Nothing ->
          case mlogin of
            Just login -> fetchUserByName login
            Nothing -> Cmd.none
      ]
    , outstandingRequests = 1
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
    ]
  )

update msg model =
  case msg of
    User (Ok (user::_)) ->
      ( { model
        | login = Just user.login
        , userId = Just user.id
        , pendingRequests = List.append model.pendingRequests
          [fetchVideos user.id]
        }
      , if (Just user.id) /= model.userId then
          Navigation.pushUrl model.navigationKey (model.location.path ++ "?userId="  ++ user.id)
        else
          Cmd.none
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Videos (Ok videos) ->
      ( { model
        | events = videos
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> List.map (\v -> {start = v.createdAt, duration = v.duration})
        }
      , Cmd.none
      )
    Videos (Err error) ->
      let _ = Debug.log "video fetch error" error in
      (model, Cmd.none)
    Response subMsg ->
      update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            }, next)
        _ -> (model, Cmd.none)
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
            | userId = Just auth.channelId
            , pendingRequests = List.append model.pendingRequests
              [fetchVideos auth.channelId]
            }
          , Cmd.none
          )
        Nothing ->
          (model, Cmd.none)
    OnContext context ->
      ( { model | theme = context.theme }, Cmd.none )
    UI (View.SetUsername username) ->
      ( { model
        | pendingRequests =
          List.append model.pendingRequests [fetchUserByName username]
        , events = []
        }
      , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (requestRate*1.05) NextRequest
    , Time.every (60 * 1000) CurrentTime
    , Browser.Events.onResize (\w h -> WindowSize (w, h))
    , TwitchExt.onAuthorized OnAuthorized
    , TwitchExt.onContext OnContext
    , MeasureText.textSize TextSize
    ]

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> Cmd Msg
fetchUserByName login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << User
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> Cmd Msg
fetchUserById id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << User
    , url = (fetchUserByIdUrl id)
    }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Response << Videos
    , url = (fetchVideosUrl userId)
    }

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
