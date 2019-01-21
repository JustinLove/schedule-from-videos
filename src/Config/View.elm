module Config.View exposing (Msg(..), document, view)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)

type Msg
  = None

css = """
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
   , h2 [] [ text "Schedule From Videos" ]
   , p [] [ text """Uses your recent Twitch videos to construct a graph by day and time showing when you have streamed in the past, and are thus likely to stream in the future. Since it uses video history, this works better for channels with extended video storage (Turbo/Prime/Partner).""" ]
   , p [] [ text """Channel ID is provided to extensions, which is then used to fetch videos from the Twitch API. There is nothing to configure.""" ]

   , displayFooter
   ]

displayFooter : Html msg
displayFooter =
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

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("../symbol-defs.svg#icon-"++name) ] [] ]

