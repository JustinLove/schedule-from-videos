module View exposing (Msg(..), Mode(..), Data(..), document, view)

import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode
import Color

type Msg
  = SetUsername String

type Mode
  = Page
  | Extension

type Data a
  = Data a
  | Unknown
  | NotFound
  | RequestFailed

css = """
body { margin: 0; overflow: hidden; }
#top { padding: 8px; }
h2 { text-align: center; margin: 0;}
.error-message { text-align: center; margin-top: 2em; }
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
svg text { animation: delay-appear 2s; }
svg polyline { animation: appear 1s; }
@keyframes appear {
  0% { opacity: 0; }
  100% { opacity: 1; }
}
@keyframes delay-appear {
  0% { opacity: 0; }
  50% { opacity: 0; }
  100% { opacity: 1; }
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


document tagger model =
  { title = "Schedule From Videos"
  , body = [Html.map tagger (view model)]
  }

view model = 
  div [ id "top", class model.theme ]
    [ node "style" [] [ text css ]
    , node "style" [] [ text <| case model.theme of
      "light" -> "body { background-color: #fff; }"
      _ -> "body { background-color: #301c2b; }"
      ]
    , case model.mode of
      Page -> 
        div []
          [ label [ for "channelname" ] [ text "Channel Name " ]
          , input
            [ type_ "text"
            , id "channelname"
            , name "channelname"
            , placeholder (case model.login of
                Data login -> login
                Unknown -> ""
                NotFound -> ""
                RequestFailed -> ""
              )
            , on "change" <| targetValue Json.Decode.string SetUsername
            ] []
          , text " "
          , case model.login of
            Data _ -> text ""
            Unknown -> text ""
            NotFound -> text "Channel name not found."
            RequestFailed -> text "User lookup failed."
          , case model.userId of
            Data _ -> text ""
            Unknown -> text ""
            NotFound -> text "Channel ID not found."
            RequestFailed -> text "User lookup failed."
          ]
      Extension ->
        h2 [] [ text "Historical Schedule" ]
    , case model.events of
      Data events ->
        displayGraph model (List.filter (\e -> e.duration < (56 * 60 * 60 * 1000)) events)
      Unknown ->
        displayGraph model []
      NotFound ->
        div [ class "error-message" ]
          [ h2 [] [ text "User ID Not Found" ]
          , p [] [ text "If you got this as url, it is most likely a copy-paste error." ]
          ]
      RequestFailed ->
        div [ class "error-message" ]
          [ h2 [] [ text "Unable to retrieve video history" ]
          , p [] [ text "Backend request failed." ]
          ]
    , displayFooter model.mode
    ]

--displayGraph : Model -> List Event -> Html msg
displayGraph model events =
  scheduleGraph
    [ Html.Attributes.style "width" "100%"
    , Html.Attributes.style "height" "auto"
    , Html.Attributes.id "graph"
    ]
    { width = (toFloat model.windowWidth)
    , height = (toFloat model.windowHeight) - (case model.mode of
        Page -> 32
        Extension -> 40
      )
    , labelWidths = model.labelWidths
    , time = model.time
    , zone = model.zone
    , days = allDays
    , events = events
    , style =
      case model.theme of 
        "light" ->
          { dataColor = Color.rgb 100 65 164
          , labelColor = Color.rgb 127 127 127
          , ruleColor = Color.rgb 178 178 178
          , currentDayColor = Color.red
          , currentTimeColor = Color.red
          }
        _ ->
          { dataColor = Color.rgb 100 65 164
          , labelColor = Color.rgb 127 127 127
          , ruleColor = Color.rgb 76 76 76
          , currentDayColor = Color.red
          , currentTimeColor = Color.red
          }
    }


displayFooter : Mode -> Html msg
displayFooter mode =
  case mode of
    Page -> pageFooter
    Extension -> extensionFooter

pageFooter : Html msg
pageFooter =
  footer []
  [ a
      [ href "https://github.com/JustinLove/schedule-from-videos"
      , target "_blank"
      , rel "noopener"
      ]
      [ icon "github", text "schedule-from-videos" ]
  , text " "
  , a
      [ href "https://twitter.com/wondible"
      , target "_blank"
      , rel "noopener"
      ]
      [ icon "twitter", text "@wondible" ]
  , text " "
  , a
    [ href "https://twitch.tv/wondible"
    , target "_blank"
    , rel "noopener"
    ]
    [ icon "twitch", text "wondible" ]
  ]

extensionFooter : Html msg
extensionFooter =
  footer []
  [ a
    [ href "https://twitch.tv/wondible"
    , target "_blank"
    , rel "noopener"
    ]
    [ text "wondible" ]
  ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
