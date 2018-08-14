module ScheduleFromVideos exposing (..)

import Twitch.Helix.Decode as Helix
import Twitch.Helix as Helix
import TwitchExt
import TwitchId
import ScheduleGraph exposing (Event)
import View

import Html
import Navigation exposing (Location)
import Http
import Time exposing (Time)
import Task
import Window

requestLimit = 100
rateLimit = 30
requestRate = 60*Time.second/rateLimit

type Msg
  = User (Result Http.Error (List Helix.User))
  | Videos (Result Http.Error (List Helix.Video))
  | Response Msg
  | NextRequest Time
  | CurrentUrl Location
  | CurrentTime Time
  | WindowSize Window.Size
  | OnAuthorized TwitchExt.Auth
  | UI (View.Msg)

type alias Model =
  { location : Location
  , login : Maybe String
  , userId : Maybe String
  , events : List Event
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , time : Time
  , windowWidth : Int
  , windowHeight : Int
  }

main = Navigation.program CurrentUrl
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : Location -> (Model, Cmd Msg)
init location =
  let
    mlogin = Debug.log "Login" <| extractSearchArgument "login" location
    muserId = Debug.log "userId" <| extractSearchArgument "userId" location
  in
  ( { location = location
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
    , time = 0
    , windowWidth = 1000
    , windowHeight = 300
    }
  , Cmd.batch
    [ Task.perform CurrentTime Time.now
    , Task.perform WindowSize Window.size
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
          Navigation.modifyUrl (model.location.pathname ++ "?userId="  ++ user.id)
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
    CurrentTime time ->
      ( {model | time = time}, Cmd.none)
    WindowSize size ->
      ( {model | windowWidth = size.width, windowHeight = size.height}, Cmd.none)
    OnAuthorized auth ->
      case String.toInt auth.channelId of
        Ok _ ->
          ( { model
            | userId = Just auth.channelId
            , pendingRequests = List.append model.pendingRequests
              [fetchVideos auth.channelId]
            }
          , Cmd.none
          )
        Err _ ->
          (model, Cmd.none)
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
    , Time.every Time.minute CurrentTime
    , Window.resizes WindowSize
    , TwitchExt.onAuthorized OnAuthorized
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
  "https://api.twitch.tv/helix/videos?first=100&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Response << Videos
    , url = (fetchVideosUrl userId)
    }

extractSearchArgument : String -> Location -> Maybe String
extractSearchArgument key location =
  location.search
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map (String.split "=")
    |> List.filter (\x -> case List.head x of
      Just s ->
        (String.toLower s) == (String.toLower key)
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
