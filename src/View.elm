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

view model = 
  div []
    [ node "style" [] [ text css ]
    , [ List.map ((videosOnDay model.videos) >> (rowHeatMap model.date)) days
        --|> List.intersperse (spacer 0 0.3)
        |> (::) (spacer 0 0.5)
        |> vertical
        |> scaleX 1000
        |> scaleY 20
      , spacer 0 3
      , displayScale 1000 20
      ]
        |> List.map (Layout.align left)
        |> vertical
        |> Collage.Render.svgExplicit
          [ Svg.Attributes.viewBox "0 0 1000 220"
          , Html.Attributes.style
            [ ("width", "100%")
            , ("height", "auto")
            ]
          ]
    ]

videosOnDay : List Video -> Day -> List Video
videosOnDay videos dow =
  List.filter (\vid -> (Date.dayOfWeek vid.createdAt) == dow) videos

rowHeatMap : Date -> List Video -> Collage Msg
rowHeatMap date videos =
  videos
    |> toRanges
    |> List.map (\(start, duration) ->
      rectangle (duration / day) 1
        |> filled (uniform Color.blue)
        |> opacity 0.3
        |> shiftX ((start / day) - 0.5)
      )
    |> (::) (segment (0, 0.7) (0, -0.7)
        |> traced (solid (0.001) (uniform Color.red))
        |> shiftX (((offset date) / day) - 0.5)
        )
    |> group

displayScale : Float -> Float -> Collage Msg
displayScale width height =
  [0, 3, 6, 9, 12, 15, 18, 21, 24]
    |> List.map (\hour ->
      [ segment (0, 0.5 * height) (0, -0.5 * height)
        |> traced (solid (0.001 * width) (uniform Color.black))
      , spacer (0.005 * width) 0
      , (fromString <| toString hour)
        |> Text.size (round height)
        |> rendered
      ]
      |> horizontal
      |> shiftX (((hour / 24) - 0.5) * width)
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
