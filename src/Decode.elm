module Decode exposing (User, Event, VideosWithName, user, videos, videosWithName)

import Json.Decode exposing (..)
import Time exposing (Posix)

type alias Event =
  { createdAt : Posix
  , duration : Int
  }

type alias User =
  { id : String
  , name : String
  }

type alias VideosWithName =
  { user : User
  , events : List Event
  }

videosWithName : Decoder VideosWithName
videosWithName =
  succeed VideosWithName
    |> map2 (|>) user
    |> map2 (|>) videos

videos : Decoder (List Event)
videos =
  field "events" (list event)

event : Decoder Event
event =
  succeed Event
    |> map2 (|>) (field "created_at" timeStamp)
    |> map2 (|>) (field "duration" duration)

user : Decoder User
user =
  field "user" userStruct

userStruct : Decoder User
userStruct =
    succeed User
      |> map2 (|>) (field "id" string)
      |> map2 (|>) (field "name" string)

duration : Decoder Int
duration = int

timeStamp : Decoder Posix
timeStamp = int |> map Time.millisToPosix
