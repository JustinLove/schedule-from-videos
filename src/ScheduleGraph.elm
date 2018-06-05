module ScheduleGraph exposing (ScheduleGraph, Style, Event, scheduleGraph, allDays)

import Html exposing (Html)
import Collage exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Text as Text exposing (..)
import Collage.Render
import Color exposing (Color)
import Svg
import Svg.Attributes
import Date exposing (Day(..))
import Time exposing (Time)

type alias ScheduleGraph =
  { width : Float
  , height : Float
  , time : Time
  , days : List Day
  , events : List Event
  , style : Style
  }


type alias Event =
  { start : Time
  , duration : Time
  }

type alias Style =
  { dataColor : Color
  , labelColor : Color
  , ruleColor : Color
  , currentDayColor : Color
  , currentTimeColor : Color
  }

day = toFloat (24 * 60 * 60 * 1000)
allDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

scheduleGraph : List (Svg.Attribute msg) -> ScheduleGraph -> Html msg
scheduleGraph attributes {width, height, time, events, days, style} =
  let
    even = height / (toFloat (List.length days) + 1)
    axis = min even (min (height/4) (width / 15))
    row = (height - axis) / (toFloat (List.length days))
    starts = List.map .start events
    first = List.minimum starts |> Maybe.withDefault 0
    timeRange = (first, (List.maximum starts |> Maybe.withDefault 0) - first)
  in
    [ days 
      |> List.map (eventsOnDay <| breakOverDays events)
      |> List.map ((rowHeatMap style timeRange)
        >> scaleX width
        >> scaleY row
        )
      |> List.map2 (contextDecorations style time) days
      |> vertical
      |> align top
    , displayScale style width height axis
      |> align top
    ]
      |> stack
      |> Collage.Render.svgExplicit
        ( ( [ -0.5 * width
          , 0
          , width
          , height
          ]
          |> List.map toString
          |> String.join " "
          |> Svg.Attributes.viewBox
          ) :: attributes
        )

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

rowHeatMap : Style -> (Time, Time) -> List Event -> Collage msg
rowHeatMap style ((first, range) as timeRange) events =
  events
    |> toRanges timeRange
    |> List.map (\(start, duration, age) ->
      rectangle (duration / day) 0.1
        |> filled (uniform style.dataColor)
        |> opacity 0.9
        |> shiftX (((start + duration/2) / day) - 0.5)
        |> shiftY ((((age + range/2) / range) - 1.0) * 0.8)
        |> scaleY 0.85
      )
    |> group
    |> setEnvelope 1 1

contextDecorations : Style -> Time -> Day -> Collage msg -> Collage msg
contextDecorations style time dow collage =
  let
    width = Layout.width collage
    height = Layout.height collage
    mark = max 1 (logBase 10 width)
    stripe = allDays
      |> List.indexedMap (\i day -> if day == dow then Just (i%2) else Nothing)
      |> List.filterMap identity
      |> List.head
      |> Maybe.withDefault 0
  in
  [ if stripe == 1 then
      rectangle width height
        |> filled (uniform style.labelColor)
        |> opacity 0.1
    else
      Layout.empty
  , (segment (0, 0.51 * height) (0, -0.51 * height)
      |> traced (solid mark (uniform style.currentTimeColor))
      |> shiftX (((offset time) / day - 0.5) * width)
      )
  , collage
    |> scaleY 0.8
  , (fromString <| toString dow)
    |> Text.size (round <| min height (width / 3))
    |> Text.color style.labelColor
    |> rendered
    |> align left
    |> shiftX (-0.5 * width + 5)
  , (if (dayOfWeek time) == dow then
      (segment (0.5 * width, 0) (-0.5 * width, 0)
        |> traced (solid mark (uniform style.currentDayColor))
        )
    else
      Layout.empty
    )
  ]
  |> group

displayScale : Style -> Float -> Float -> Float -> Collage msg
displayScale style width height line =
  let mark = max ultrathin ((logBase 10 width) * 0.55) in
  [0, 3, 6, 9, 12, 15, 18, 21, 24]
    |> List.map (\hour ->
      [ segment (0, height) (0, 0)
        |> traced (solid mark (uniform style.ruleColor))
      , spacer (0.005 * width) 0
      , (fromString <| toString hour)
        |> Text.size (round <| min line (width / 15))
        |> Text.color style.labelColor
        |> rendered
        |> shiftY (line / 2)
      ]
      |> horizontal
      |> shiftX (((hour / 24) - 0.5) * width)
    )
    |> group

toRanges : (Time, Time) -> List Event -> List (Time, Time, Time)
toRanges timeRange =
  List.map (toRange timeRange)

toRange : (Time, Time) -> Event -> (Time, Time, Time)
toRange (first, range) event =
  ( offset event.start
  , event.duration
  , first + range - event.start
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
