module ScheduleFromVideos exposing (..)

import Twitch.Deserialize exposing (Video)
import Twitch exposing (helix)
import TwitchId
import View

import Html
import Http
import Time

requestLimit = 100
rateLimit = 30
requestRate = 60*Time.second/rateLimit

type Msg
  = User (Result Http.Error (List Twitch.Deserialize.User))
  | Videos (Result Http.Error (List Video))
  | Response Msg
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { userId : Maybe String
  , videos : List Video
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init =
  ( { userId = Nothing
    , videos = []
    , pendingRequests = [fetchUser "wondible"]
    , outstandingRequests = 1
    }
  , Cmd.none
  )

update msg model =
  case msg of
    User (Ok (user::_)) ->
      ( { model
        | userId = Just user.id
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
        | videos = List.append model.videos videos
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
    UI (View.None) ->
      ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (requestRate*1.05) NextRequest
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
