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
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , text "view"
    , ul []
      <| (toRanges model.videos
      |> List.map (\(start, end) -> li []
      [ text <| toString start
      , text " - "
      , text <| toString end
      ]))
    ]

toRanges : List Video -> List (Time, Time)
toRanges =
  List.map toRange

toRange : Video -> (Time, Time)
toRange video =
  ( offset video.createdAt
  , (offset video.createdAt) + video.duration
  )
  
offset : Date -> Time
offset date =
  toFloat (0
  + (Date.hour date) * 60 * 60
  + (Date.minute date) * 60
  + (Date.second date)
  ) * 1000
