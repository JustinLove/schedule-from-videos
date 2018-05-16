module ScheduleGraph exposing (ScheduleGraph, Event, scheduleGraph, allDays)

import Html exposing (Html)
import Html.Attributes
import Collage exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Text as Text exposing (..)
import Collage.Render
import Color
import Svg.Attributes
import Date exposing (Day(..))
import Time exposing (Time)

type alias ScheduleGraph =
  { width : Float
  , height : Float
  , time : Time
  , days : List Day
  , events : List Event
  }

type alias Event =
  { start : Time
  , duration : Time
  }

day = toFloat (24 * 60 * 60 * 1000)
allDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
twitchPurple = Color.rgb 100 65 164
labelColor = Color.greyscale 0.7

scheduleGraph : ScheduleGraph -> Html msg
scheduleGraph {width, height, time, events, days} =
  let
    even = height / (toFloat (List.length days) + 1)
    axis = min even (min (height/4) (width / 15))
    row = (height - axis) / (toFloat (List.length days))
  in
    [ days 
      |> List.map (eventsOnDay <| breakOverDays events)
      |> List.map (rowHeatMap
        >> scaleX width
        >> scaleY row
        )
      |> List.map2 (contextDecorations time) days
      |> vertical
      |> align top
    , displayScale width height axis
      |> align top
    ]
      |> stack
      |> Collage.Render.svgExplicit
        [ [ -0.5 * width
          , 0
          , width
          , height
          ]
          |> List.map toString
          |> String.join " "
          |> Svg.Attributes.viewBox
        , Html.Attributes.style
          [ ("width", "100%")
          , ("height", "auto")
          ]
        ]

breakOverDays : List Event -> List Event
breakOverDays =
  List.concatMap breakEvent

breakEvent : Event -> List Event
breakEvent event =
  let
    start = offset event.start
    remaining = day - start
  in
  if event.duration > remaining then
    {event | duration = remaining} :: breakEvent
      { event
      | start = event.start + remaining
      , duration = event.duration - remaining
      }
  else
    [event]

eventsOnDay : List Event -> Day -> List Event
eventsOnDay events dow =
  List.filter (\vid -> (dayOfWeek vid.start) == dow) events

rowHeatMap : List Event -> Collage msg
rowHeatMap events =
  events
    |> toRanges
    |> List.map (\(start, duration) ->
      rectangle (duration / day) 1
        |> filled (uniform twitchPurple )
        |> opacity 0.3
        |> shiftX (((start + duration/2) / day) - 0.5)
      )
    |> group
    |> setEnvelope 1 1

contextDecorations : Time -> Day -> Collage msg -> Collage msg
contextDecorations time dow collage =
  let
    width = Layout.width collage
    height = Layout.height collage
  in
  [ (segment (0, 0.5 * height) (0, -0.5 * height)
      |> traced (solid 1 (uniform Color.red))
      |> shiftX (((offset time) / day - 0.5) * width)
      )
  , collage
    |> scaleY 0.8
  , (fromString <| toString dow)
    |> Text.size (round <| min height (width / 3))
    |> Text.color labelColor
    |> rendered
    |> align left
    |> shiftX (-0.5 * width + 5)
  , (if (dayOfWeek time) == dow then
      rectangle width height
        |> filled (uniform Color.black)
        |> opacity 0.1
    else
      Layout.empty
    )
  ]
  |> group

displayScale : Float -> Float -> Float -> Collage msg
displayScale width height line =
  [0, 3, 6, 9, 12, 15, 18, 21, 24]
    |> List.map (\hour ->
      [ segment (0, height) (0, 0)
        |> traced (solid (0.001 * width) (uniform <| Color.greyscale 0.3))
      , spacer (0.005 * width) 0
      , (fromString <| toString hour)
        |> Text.size (round <| min line (width / 15))
        |> Text.color labelColor
        |> rendered
        |> shiftY (line / 2)
      ]
      |> horizontal
      |> shiftX (((hour / 24) - 0.5) * width)
    )
    |> group

toRanges : List Event -> List (Time, Time)
toRanges =
  List.map toRange

toRange : Event -> (Time, Time)
toRange event =
  ( offset event.start
  , event.duration
  )

dayOfWeek : Time -> Day
dayOfWeek =
  Date.fromTime >> Date.dayOfWeek

offset : Time -> Time
offset time =
  let date = Date.fromTime time in
  toFloat (0
  + (Date.hour date) * 60 * 60
  + (Date.minute date) * 60
  + (Date.second date)
  ) * 1000

setEnvelope : Float -> Float -> Collage msg -> Collage msg
setEnvelope width height collage =
  impose collage (spacer width height)
