module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (Video)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Date exposing (Date)
import Time exposing (Time)

type Msg
  = None

css = """
.row {position: relative;}
.stream {background: blue; position: absolute; opacity: 0.1; height: 1em;}
"""

day = toFloat (24 * 60 * 60 * 1000)

view model = 
  div []
    [ node "style" [] [ text css ]
    , rowHeatMap model.videos
    ]

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
    [ ]))

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
