module View exposing (Msg(..), view)

import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Date exposing (Day(..))
import Json.Decode

type Msg
  = SetUsername String

css = """
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , label [ for "twitchname" ] [ text "Twitch User" ]
    , input
      [ type_ "text"
      , id "twitchname"
      , name "twitchname"
      , on "change" <| targetValue Json.Decode.string SetUsername
      ] []
    , scheduleGraph
      [ Html.Attributes.style
        [ ("width", "100%")
        , ("height", "auto")
        ]
      ]
      { width = (toFloat model.windowWidth)
      , height = (toFloat model.windowHeight) - 20
      , time = model.time
      , days = allDays
      , events = model.events
      }
    ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
