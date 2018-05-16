module ScheduleFromVideos exposing (..)

import Twitch.Deserialize
import Twitch exposing (helix)
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
  = User (Result Http.Error (List Twitch.Deserialize.User))
  | Videos (Result Http.Error (List Twitch.Deserialize.Video))
  | Response Msg
  | NextRequest Time
  | CurrentUrl Location
  | CurrentTime Time
  | WindowSize Window.Size
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
    mid = Debug.log "id" <| extractSearchArgument "id" location
  in
  ( { location = location
    , login = mlogin
    , userId = mid
    , events = []
    , pendingRequests = [
      case mid of
        Just id -> fetchVideos id
        Nothing ->
          case mlogin of
            Just login -> fetchUser login
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
      , Cmd.none
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Videos (Ok videos) ->
      ( { model
        | events = List.map (\v -> {start = v.createdAt, duration = v.duration}) videos
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
    UI (View.SetUsername username) ->
      ( { model
        | pendingRequests =
          List.append model.pendingRequests [fetchUser username]
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
    ]

fetchUserUrl : String -> String
fetchUserUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUser : String -> Cmd Msg
fetchUser login =
  helix <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Twitch.Deserialize.users
    , tagger = Response << User
    , url = (fetchUserUrl login)
    }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  helix <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Twitch.Deserialize.videos
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
        s == key
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
