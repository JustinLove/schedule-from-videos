module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (Video)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Text as Text exposing (..)
import Collage.Render
import Color
import Svg.Attributes
import Date exposing (Date, Day(..))
import Time exposing (Time)

type Msg
  = None

css = """
"""

day = toFloat (24 * 60 * 60 * 1000)
days = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
twitchPurple = Color.rgb 100 65 164

view model = 
  div []
    [ node "style" [] [ text css ]
    , [ days 
        |> List.map (videosOnDay model.videos)
        |> List.map (rowHeatMap
          >> scaleX 1000
          >> scaleY 20
          )
        |> List.map2 (contextDecorations model.date) days
        |> (::) (spacer 0 10)
        |> vertical
        |> shiftY (20 * 7.0)
      , displayScale 1000 20
      ]
        |> stack
        |> Collage.Render.svgExplicit
          [ Svg.Attributes.viewBox "-500 -140 1000 170"
          , Html.Attributes.style
            [ ("width", "100%")
            , ("height", "auto")
            ]
          ]
    ]

videosOnDay : List Video -> Day -> List Video
videosOnDay videos dow =
  List.filter (\vid -> (Date.dayOfWeek vid.createdAt) == dow) videos

rowHeatMap : List Video -> Collage Msg
rowHeatMap videos =
  videos
    |> toRanges
    |> List.map (\(start, duration) ->
      rectangle (duration / day) 1
        |> filled (uniform twitchPurple )
        |> opacity 0.3
        |> shiftX (((start + duration/2) / day) - 0.5)
      )
    |> group
    |> setEnvelope 1 1

contextDecorations : Date -> Day -> Collage Msg -> Collage Msg
contextDecorations date dow collage =
  let
    width = Layout.width collage
    height = Layout.height collage
  in
  [ (segment (0, 0.5 * height) (0, -0.5 * height)
      |> traced (solid 1 (uniform Color.red))
      |> shiftX (((offset date) / day - 0.5) * width)
      )
  , (fromString <| toString dow)
    |> Text.size (round height)
    |> rendered
    |> Layout.align left
    |> shiftX (-0.5 * width + 5)
  , collage
    |> scaleY 0.8
  , (if (Date.dayOfWeek date) == dow then
      rectangle width height
        |> filled (uniform Color.black)
        |> opacity 0.1
    else
      Layout.empty
    )
  ]
  |> group

displayScale : Float -> Float -> Collage Msg
displayScale width height =
  [0, 3, 6, 9, 12, 15, 18, 21, 24]
    |> List.map (\hour ->
      [ segment (0, 7.5 * height) (0, -0.5 * height)
        |> traced (solid (0.001 * width) (uniform <| Color.greyscale 0.3))
      , spacer (0.005 * width) 0
      , (fromString <| toString hour)
        |> Text.size (round height)
        |> rendered
      ]
      |> horizontal
      |> shiftX (((hour / 24) - 0.5) * width)
      |> shiftY (-height)
    )
    |> group

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

setEnvelope : Float -> Float -> Collage msg -> Collage msg
setEnvelope width height collage =
  impose collage (spacer width height)
