module Decode exposing (Event, events)

import Duration

import Json.Decode exposing (..)
import Iso8601
import Time exposing (Posix)

type alias Event =
  { createdAt : Posix
  , duration : Int
  }

events : Decoder (List Event)
events =
  field "events" (list event)

event : Decoder Event
event =
  succeed Event
    |> map2 (|>) (field "created_at" timeStamp)
    |> map2 (|>) (field "duration" duration)

duration : Decoder Int
duration = Duration.decoder

timeStamp : Decoder Posix
timeStamp = Iso8601.decoder
