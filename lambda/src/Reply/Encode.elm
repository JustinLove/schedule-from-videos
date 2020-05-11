module Reply.Encode exposing (videos, user)

import Twitch.Helix.Decode as Helix exposing (Video)

import Json.Encode exposing (..)
import Time

videos : List Video -> Value
videos = list video

video : Video -> Value
video v =
  object
    [ ("created_at", v.createdAt |> Time.posixToMillis |> int)
    , ("duration", v.duration |> int)
    ]

user : String -> String -> Value
user userId userName =
  object
    [ ("id", string userId)
    , ("name", string userName)
    ]

