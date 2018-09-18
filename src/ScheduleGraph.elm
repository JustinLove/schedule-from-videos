module ScheduleGraph exposing (ScheduleGraph, Style, Event, scheduleGraph, allDays)

import Html exposing (Html)
import Collage exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Text as Text exposing (..)
import Collage.Render
import Color exposing (Color)
import Svg
import Svg.Attributes
import Time exposing
  ( Weekday(..)
  , Month(..)
  , Posix
  , Zone
  , posixToMillis
  , millisToPosix
  )

type alias ScheduleGraph =
  { width : Float
  , height : Float
  , time : Posix
  , zone : Zone
  , days : List Weekday
  , events : List Event
  , style : Style
  }

type alias Event =
  { start : Posix
  , duration : Duration
  }

type alias Duration = Int
type alias Offset = Int

type alias Style =
  { dataColor : Color
  , labelColor : Color
  , ruleColor : Color
  , currentDayColor : Color
  , currentTimeColor : Color
  }

day = (24 * 60 * 60 * 1000)
allDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
dayName dow =
  case dow of
    Mon -> "Mon"
    Tue -> "Tue"
    Wed -> "Wed"
    Thu -> "Thu"
    Fri -> "Fri"
    Sat -> "Sat"
    Sun -> "Sun"
dayStripe dow =
  case dow of
    Mon -> False
    Tue -> True
    Wed -> False
    Thu -> True
    Fri -> False
    Sat -> True
    Sun -> False
monthNum mon =
  case mon of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"
numberPad : Int -> String
numberPad i =
  if i < 10 then
    "0" ++ (String.fromInt i)
  else
    String.fromInt i

scheduleGraph : List (Svg.Attribute msg) -> ScheduleGraph -> Html msg
scheduleGraph attributes {width, height, time, zone, events, days, style} =
  let
    even = height / (toFloat (List.length days) + 1)
    axis = min even (min (height/4) (width / 15))
    row = (height - axis) / (toFloat (List.length days))
    starts = List.map (.start>>posixToMillis) events
    first = List.minimum starts |> Maybe.withDefault 0
    timeRange = (millisToPosix first, (List.maximum starts |> Maybe.withDefault 0) - first)
  in
    [ days 
      |> List.map (eventsOnDay zone <| breakOverDays zone events)
      |> List.map ((rowHeatMap style zone timeRange)
        >> scaleX width
        >> scaleY row
        )
      |> List.map2 (contextDecorations style zone time) days
      |> vertical
      |> align top
    , displayScale zone timeRange style width height axis
      |> align top
    ]
      |> stack
      |> Collage.Render.svgExplicit
        ( ( [ -0.5 * width
          , 0
          , width
          , height
          ]
          |> List.map String.fromFloat
          |> String.join " "
          |> Svg.Attributes.viewBox
          ) :: attributes
        )

breakOverDays : Zone -> List Event -> List Event
breakOverDays zone =
  List.concatMap (breakEvent zone)

breakEvent : Zone -> Event -> List Event
breakEvent zone event =
  let
    start = offset zone event.start
    remaining = day - start
  in
  if event.duration > remaining then
    {event | duration = remaining} :: (breakEvent zone)
      { event
      | start = millisToPosix ((posixToMillis event.start) + remaining)
      , duration = event.duration - remaining
      }
  else
    [event]

eventsOnDay : Zone -> List Event -> Weekday -> List Event
eventsOnDay zone events dow =
  List.filter (\vid -> (Time.toWeekday zone vid.start) == dow) events

rowHeatMap : Style -> Zone -> (Posix, Duration) -> List Event -> Collage msg
rowHeatMap style zone ((first, range) as timeRange) events =
  events
    |> toRanges zone timeRange
    |> List.map (\(start, duration, age) ->
      rectangle (duration / day) 0.1
        |> filled (uniform style.dataColor)
        |> opacity 0.9
        |> shiftX (((start + duration/2) / day) - 0.5)
        |> shiftY ((age / (toFloat range)) - 0.5)
        |> scaleY 0.70
      )
    |> group
    |> setEnvelope 1 1

contextDecorations : Style -> Zone -> Posix -> Weekday -> Collage msg -> Collage msg
contextDecorations style zone time dow collage =
  let
    width = Layout.width collage
    height = Layout.height collage
    mark = max 1 (logBase 10 width)
  in
  [ if dayStripe dow then
      rectangle width height
        |> filled (uniform style.labelColor)
        |> opacity 0.1
    else
      Layout.empty
  , (segment (0, 0.51 * height) (0, -0.51 * height)
      |> traced (solid mark (uniform style.currentTimeColor))
      |> shiftX ((toFloat (offset zone time) / day - 0.5) * width)
      )
  , collage
    |> scaleY 0.8
  , (fromString <| dayName dow)
    |> Text.size (round <| min height (width / 3))
    |> Text.color style.labelColor
    |> rendered
    |> align left
    |> shiftX (-0.5 * width + 5)
  , (if (Time.toWeekday zone time) == dow then
      (segment (0.5 * width, 0) (-0.5 * width, 0)
        |> traced (solid mark (uniform style.currentDayColor))
        )
    else
      Layout.empty
    )
  ]
  |> group

displayScale : Zone -> (Posix, Duration) -> Style -> Float -> Float -> Float -> Collage msg
displayScale zone (first, range) style width height line =
  let
    startDate = first
    start = (String.fromInt <| Time.toYear zone startDate) ++ "-" ++
            (monthNum <| Time.toMonth zone startDate) ++ "-" ++
            (numberPad <| Time.toDay zone startDate)
    endDate = millisToPosix ((posixToMillis first) + range)
    end = (String.fromInt <| Time.toYear zone endDate) ++ "-" ++
          (monthNum <| Time.toMonth zone endDate) ++ "-" ++
          (numberPad <| Time.toDay zone endDate)
  in
  [ displayTimeScale style width height line
  , (fromString start)
    |> Text.size (round <| line/4)
    |> Text.color style.labelColor
    |> rendered
    |> shiftY (height - line/4)
    |> shiftX (-width/4)
  , (fromString end)
    |> Text.size (round <| line/4)
    |> Text.color style.labelColor
    |> rendered
    |> shiftY (line * 1.1)
    |> shiftX (width * 0.4)
  ] |> group

displayTimeScale : Style -> Float -> Float -> Float -> Collage msg
displayTimeScale style width height line =
  let mark = max ultrathin ((logBase 10 width) * 0.55) in
  [0, 3, 6, 9, 12, 15, 18, 21, 24]
    |> List.map (\hour ->
      [ segment (0, height) (0, 0)
        |> traced (solid mark (uniform style.ruleColor))
      , spacer (0.005 * width) 0
      , (fromString <| String.fromFloat hour)
        |> Text.size (round <| min line (width / 15))
        |> Text.color style.labelColor
        |> rendered
        |> shiftY (line / 2)
      ]
      |> horizontal
      |> shiftX (((hour / 24) - 0.5) * width)
    )
    |> group

toRanges : Zone -> (Posix, Duration) -> List Event -> List (Float, Float, Float)
toRanges zone timeRange =
  List.map (toRange zone timeRange)

toRange : Zone -> (Posix, Duration) -> Event -> (Float, Float, Float)
toRange zone (first, range) event =
  ( offset zone event.start |> toFloat
  , event.duration |> toFloat
  , (posixToMillis first) + range - (posixToMillis event.start) |> toFloat
  )

offset : Zone -> Posix -> Offset
offset zone time =
  (0
  + (Time.toHour zone time) * 60 * 60
  + (Time.toMinute zone time) * 60
  + (Time.toSecond zone time)
  ) * 1000

setEnvelope : Float -> Float -> Collage msg -> Collage msg
setEnvelope width height collage =
  impose collage (spacer width height)
