module Reply.Encode exposing (videosReply, videosWithNameReply, userReply)

import Twitch.Helix.Decode as Helix exposing (Video)

import Json.Encode exposing (..)
import Time

videosReply : {events : List Video} -> Value
videosReply {events} =
  object
    [ ("events", videos events)
    ]

videosWithNameReply :
  { user : { id : String, name : String }
  , events : List Video
  } -> Value
videosWithNameReply {user, events} =
  object
    [ ("user", encodeUser user.id user.name)
    , ("events", videos events)
    ]

userReply : { user : { id : String, name : String } } -> Value
userReply {user} =
  object
    [ ("user", encodeUser user.id user.name)
    ]

videos : List Video -> Value
videos = list video

video : Video -> Value
video v =
  object
    [ ("created_at", v.createdAt |> Time.posixToMillis |> int)
    , ("duration", v.duration |> int)
    ]

encodeUser : String -> String -> Value
encodeUser userId userName =
  object
    [ ("id", string userId)
    , ("name", string userName)
    ]

