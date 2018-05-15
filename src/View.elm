module View exposing (Msg(..), view)

import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Day(..))

type Msg
  = None

css = """
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , scheduleGraph <|
      { width = (toFloat model.windowWidth)
      , height = (toFloat model.windowHeight)
      , time = model.time
      , days = allDays
      , videos = model.videos
      }
    ]
