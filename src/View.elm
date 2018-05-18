module View exposing (Msg(..), view)

import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Date exposing (Day(..))
import Json.Decode
import Color

type Msg
  = SetUsername String

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , label [ for "channelname" ] [ text "Channel Name" ]
    , input
      [ type_ "text"
      , id "channelname"
      , name "channelname"
      , placeholder (Maybe.withDefault "" model.login)
      , on "change" <| targetValue Json.Decode.string SetUsername
      ] []
    , scheduleGraph
      [ Html.Attributes.style
        [ ("width", "98%")
        , ("height", "auto")
        ]
      ]
      { width = (toFloat model.windowWidth)
      , height = (toFloat model.windowHeight) - 20
      , time = model.time
      , days = allDays
      , events = List.filter (\e -> e.duration < (56 * 60 * 60 * 1000)) model.events
      , style =
        { dataColor = Color.rgb 100 65 164
        , labelColor = Color.greyscale 0.5
        , ruleColor = Color.greyscale 0.6
        , currentDayColor = Color.white
        , currentTimeColor = Color.red
        }
      }
    ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
