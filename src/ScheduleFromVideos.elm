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
  = Videos (Result Http.Error (List Video))
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { videos : List Video
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
  ( { videos = []
    , pendingRequests = []
    , outstandingRequests = 0
    }
  , Cmd.none
  )

update msg model =
  case msg of
    Videos (Ok videos) ->
      ( { model
        | videos = List.append model.videos videos
        }
      , Cmd.none
      )
    Videos (Err error) ->
      let _ = Debug.log "video fetch error" error in
      (model, Cmd.none)
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

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&user_id=" ++ userId

fetchVideos : Maybe String -> String -> Cmd Msg
fetchVideos auth userId =
  helix <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Twitch.Deserialize.videos
    , tagger = Videos
    , url = (fetchVideosUrl userId)
    }
