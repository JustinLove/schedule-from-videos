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
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query
import Http
import Time exposing (Posix, Zone)
import Task

type Msg
  = User (Result Http.Error (List Helix.User))
  | Game (Result Http.Error (List Helix.Game))
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
  , login : Maybe String
  , userId : Maybe String
  , gamename : Maybe String
  , gameId : Maybe String
  , events : List Event
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
    mgamename = extractSearchArgument "gamename" location
    mgameId = extractSearchArgument "gameId" location
    manchor = extractSearchArgument "anchor" location
  in
  ( { location = location
    , navigationKey = key
    , clientId = TwitchId.clientId
    , login = mlogin
    , userId = muserId
    , gamename = mgamename
    , gameId = mgameId
    , events = []
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
        Just userId -> fetchUserById TwitchId.clientId userId
        Nothing ->
          case mlogin of
            Just login -> fetchUserByName TwitchId.clientId login
            Nothing ->
              case mgameId of
                Just gameId -> fetchGameById TwitchId.clientId gameId
                Nothing ->
                  case mgamename of
                    Just gamename -> fetchGameByName TwitchId.clientId gamename
                    Nothing -> Cmd.none
    ]
  )

update msg model =
  case msg of
    User (Ok (user::_)) ->
      let
        m2 =
          { model
          | login = Just user.login
          , userId = Just user.id
          }
      in
      ( m2
      , Cmd.batch
        [ fetchVideosByUser model.clientId user.id
        , if (Just user.id) /= model.userId then
            Navigation.pushUrl model.navigationKey (createPath m2)
          else
            Cmd.none
        ]
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Game (Ok (game::_)) ->
      let
        m2 =
          { model
          | gamename = Just game.name
          , gameId = Just game.id
          }
      in
      ( m2
      , Cmd.batch
        [ fetchVideosByGame model.clientId game.id
        , if (Just game.id) /= model.gameId then
            Navigation.pushUrl model.navigationKey (createPath m2)
          else
            Cmd.none
        ]
      )
    Game (Ok _) ->
      let _ = Debug.log "game did not find that name" "" in
      (model, Cmd.none)
    Game (Err error) ->
      let _ = Debug.log "game fetch error" error in
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
            , userId = Just auth.channelId
            }
          , fetchVideosByUser auth.clientId auth.channelId
          )
        Nothing ->
          (model, Cmd.none)
    OnContext context ->
      ( { model | theme = context.theme }, Cmd.none )
    UI (View.SetUsername username) ->
      ( { model | events = [] }
      , fetchUserByName model.clientId username)
    UI (View.SetGamename gamename) ->
      ( { model | events = [] }
      , fetchGameByName model.clientId gamename)

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
    , tagger = User
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
    , tagger = User
    , url = (fetchUserByIdUrl id)
    }

fetchGameByNameUrl : String -> String
fetchGameByNameUrl name =
  "https://api.twitch.tv/helix/games?name=" ++ name

fetchGameByName : String -> String -> Cmd Msg
fetchGameByName clientId name =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.games
    , tagger = Game
    , url = (fetchGameByNameUrl name)
    }

fetchGameByIdUrl : String -> String
fetchGameByIdUrl id =
  "https://api.twitch.tv/helix/games?id=" ++ id

fetchGameById : String -> String -> Cmd Msg
fetchGameById clientId id =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.games
    , tagger = Game
    , url = (fetchGameByIdUrl id)
    }


fetchVideosByUserUrl : String -> String
fetchVideosByUserUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideosByUser : String -> String -> Cmd Msg
fetchVideosByUser clientId userId =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Videos
    , url = (fetchVideosByUserUrl userId)
    }

fetchVideosByGameUrl : String -> String
fetchVideosByGameUrl gameId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&game_id=" ++ gameId

fetchVideosByGame : String -> String -> Cmd Msg
fetchVideosByGame clientId gameId =
  Helix.send <|
    { clientId = clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Videos
    , url = (fetchVideosByGameUrl gameId)
    }

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing


createQueryString : Model -> List Url.QueryParameter
createQueryString model =
  [ Maybe.map (Url.string "userId") model.userId
  , Maybe.map (Url.string "login") model.login
  , Maybe.map (Url.string "gameId") model.gameId
  , Maybe.map (Url.string "gamename") model.gamename
  ]
    |> List.filterMap identity

createPath : Model -> String
createPath model =
  Url.relative [] (createQueryString model)
