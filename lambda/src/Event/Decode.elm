module Event.Decode exposing
  ( event
  , Event(..)
  )

import Json.Decode exposing (..)

type Event
  = Videos { userId : String }

event : Decoder Event
event =
  (field "event" string)
    |> andThen (\e -> case e of
      "videos" ->
        (field "user_id" string)
          |> map (\u -> {userId = u})
          |> map Videos
      _ ->
        fail ("unknown event " ++ e)
    )
