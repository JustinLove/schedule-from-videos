module Decode exposing
  ( User
  , users
  , Event
  , events
  )

import Twitch.Helix as Helix
import Twitch.Helix.User as User
import Twitch.Helix.Video as Video

import Json.Decode exposing (..)
import Time exposing (Posix)

type alias User =
  { id : Helix.UserId
  , displayName : String
  }

users : Decoder (List User)
users = User.response user

user : Decoder User
user =
  map2 User
    User.id
    User.displayName

type alias Event =
  { createdAt : Posix
  , duration : Int
  }

events : Decoder (List Event)
events =
  Video.response video
    |> map (List.filterMap (\(videoType, ev) -> if videoType == Video.Archive then Just ev else Nothing))

video : Decoder (Video.VideoType, Event)
video =
  map2 Tuple.pair
    Video.videoType
    event

event : Decoder Event
event =
  map2 Event
    Video.createdAt
    Video.duration
