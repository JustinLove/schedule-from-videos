module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (Video)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Date exposing (Date, Day(..))
import Time exposing (Time)

type Msg
  = None

css = """
.row {position: relative; height: 1em; margin: 0.2em;}
.stream {background: blue; position: absolute; opacity: 0.3; height: 1em;}
.label {position: absolute; height: 1em; border-left: solid 1px;}
"""

day = toFloat (24 * 60 * 60 * 1000)
days = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

view model = 
  div []
    [ node "style" [] [ text css ]
    , div []
      <| List.map ((videosOnDay model.videos) >> rowHeatMap) days
    , displayScale
    ]

videosOnDay : List Video -> Day -> List Video
videosOnDay videos dow =
  List.filter (\vid -> (Date.dayOfWeek vid.createdAt) == dow) videos

rowHeatMap : List Video -> Html Msg
rowHeatMap videos =
  div [ class "row" ]
    <| (toRanges videos
    |> List.map (\(start, duration) -> div
    [ class "stream"
    , style
      [ ("left", (toString (start * 90 / day)) ++ "%")
      , ("width", (toString (duration * 90 / day)) ++ "%")
      ]
    ]
    []))

displayScale : Html Msg
displayScale =
  div [ class "row" ]
    <| ([0, 3, 6, 9, 12, 15, 18, 21, 24]
      |> List.map (\hour -> div
      [ class "label"
      , style
        [ ("left", (toString (hour * 90 / 24)) ++ "%")
        ]
      ]
      [ text <| toString hour
      ]))

toRanges : List Video -> List (Time, Time)
toRanges =
  List.map toRange

toRange : Video -> (Time, Time)
toRange video =
  ( offset video.createdAt
  , video.duration
  )
  
offset : Date -> Time
offset date =
  toFloat (0
  + (Date.hour date) * 60 * 60
  + (Date.minute date) * 60
  + (Date.second date)
  ) * 1000
