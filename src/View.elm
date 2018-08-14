module View exposing (Msg(..), Mode(..), view)

import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Date exposing (Day(..))
import Json.Decode
import Color

type Msg
  = SetUsername String

type Mode
  = Page
  | Extension

css = """
body { margin: 0; }
#top { padding: 8px; }
h2 { text-align: center; margin: 0;}
footer { position: fixed; bottom: 0;}
svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}

#top.dark {
  background-color: #301c2b;
  color: #e5e3e8;
}
.dark .icon-github { color: #888; }
.dark .icon-twitter { color: #55acee; }
.dark .icon-twitch { color: #6441A4; }
.dark a:link, .dark a:visited { color: #6441a4; }
.dark a:hover, .dark a:active { color: rgb(218, 216, 222); }

#top.light {
  background-color: #fff;
  color: #232127;
}
.light .icon-github { color: #888; }
.light .icon-twitter { color: #55acee; }
.light .icon-twitch { color: #e2dbf0; }
.light a:link, .light a:visited { color: #e2dbf0; }
.light a:hover, .light a:active { color: rgb(218, 216, 222); }
"""

view model = 
  div [ id "top", class model.theme ]
    [ node "style" [] [ text css ]
    , case model.mode of
      Page -> 
        div []
          [ label [ for "channelname" ] [ text "Channel Name " ]
          , input
            [ type_ "text"
            , id "channelname"
            , name "channelname"
            , placeholder (Maybe.withDefault "" model.login)
            , on "change" <| targetValue Json.Decode.string SetUsername
            ] []
          ]
      Extension ->
        h2 [] [ text "Historical Schedule" ]
    , scheduleGraph
      [ Html.Attributes.style
        [ ("width", "98%")
        , ("height", "auto")
        ]
      ]
      { width = (toFloat model.windowWidth)
      , height = (toFloat model.windowHeight) - (case model.mode of
          Page -> 20
          Extension -> 28
        )
      , time = model.time
      , days = allDays
      , events = List.filter (\e -> e.duration < (56 * 60 * 60 * 1000)) model.events
      , style =
        case model.theme of 
          "light" ->
            { dataColor = Color.rgb 100 65 164
            , labelColor = Color.greyscale 0.5
            , ruleColor = Color.greyscale 0.3
            , currentDayColor = Color.red
            , currentTimeColor = Color.red
            }
          _ ->
            { dataColor = Color.rgb 100 65 164
            , labelColor = Color.greyscale 0.5
            , ruleColor = Color.greyscale 0.7
            , currentDayColor = Color.red
            , currentTimeColor = Color.red
            }
      }
    , displayFooter model.mode
    ]

displayFooter : Mode -> Html msg
displayFooter mode =
  footer []
  [ case mode of
    Page ->
      a [ href "https://github.com/JustinLove/schedule-from-videos" ]
        [ icon "github", text "schedule-from-videos" ]
    Extension -> text ""
  , text " "
  , case mode of
    Page ->
      a [ href "https://twitter.com/wondible" ]
        [ icon "twitter", text "@wondible" ]
    Extension -> text ""
  , text " "
  , a [ href "https://twitch.tv/wondible" ]
    [ icon "twitch", text "wondible" ]
  ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("#icon-"++name) ] [] ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
