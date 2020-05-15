module ScheduleFromVideos exposing (..)

import Backend
import Decode
import MeasureText
import TwitchExt
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
  = UserForName (Result Http.Error (Decode.User))
  | Videos (Result Http.Error (List Decode.Event))
  | VideosWithName (Result Http.Error (Decode.VideosWithName))
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
          fetchVideosWithName id
        Nothing ->
          case mlogin of
            Just login -> fetchUserByName login
            Nothing -> Cmd.none
    ]
  )

update msg model =
  case msg of
    UserForName (Ok user) ->
      ( { model
        | login = Data user.name
        , userId = Data user.id
        }
      , Cmd.batch
        [ fetchVideos user.id
        , if (Data user.id) /= model.userId then
            Navigation.pushUrl model.navigationKey (model.location.path ++ "?userId="  ++ user.id)
          else
            Cmd.none
        ]
      )
    UserForName (Err error) ->
      ({ model | login = RequestFailed, userId = Unknown }, Cmd.none)
    Videos (Ok videos) ->
      ( { model
        | events = videos
          |> List.map (\v -> {start = v.createdAt, duration = v.duration})
          |> Data
        }
      , Cmd.none
      )
    Videos (Err error) ->
      ({ model | events = RequestFailed }, Cmd.none)
    VideosWithName (Ok {user, events}) ->
      ( { model
        | events = events
          |> List.map (\v -> {start = v.createdAt, duration = v.duration})
          |> Data
        , login = Data user.name
        , userId = Data user.id
        }
      , Cmd.none
      )
    VideosWithName (Err error) ->
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
            | userId = Data auth.channelId
            }
          , fetchVideos auth.channelId
          )
        Nothing ->
          (model, Cmd.none)
    OnContext context ->
      ( { model | theme = context.theme }, Cmd.none )
    UI (View.SetUsername username) ->
      ( { model |events = Unknown }
      , fetchUserByName username)

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
  Backend.url ++ "/user/" ++ login

fetchUserByName : String -> Cmd Msg
fetchUserByName login =
  Http.get
    { url = (fetchUserByNameUrl login)
    , expect = Http.expectJson UserForName Decode.user
    }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  Backend.url ++ "/videos/" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  Http.get
    { url = (fetchVideosUrl userId)
    , expect = Http.expectJson Videos Decode.videos
    }

fetchVideosWithNameUrl : String -> String
fetchVideosWithNameUrl userId =
  Backend.url ++ "/videoswithname/" ++ userId

fetchVideosWithName : String -> Cmd Msg
fetchVideosWithName userId =
  Http.get
    { url = (fetchVideosWithNameUrl userId)
    , expect = Http.expectJson VideosWithName Decode.videosWithName
    }

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
