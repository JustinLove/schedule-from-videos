port module Lambda exposing
  ( Event(..)
  , event
  , test
  )

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode

type Event
  = Videos String

event : (Result Decode.Error Event -> msg) -> Sub msg
event tagger =
  lambdaEvent (decodeEvent >> tagger)

decodeEvent : Value -> Result Decode.Error Event
decodeEvent thing =
  Decode.decodeValue eventDecoder thing
    |> Result.mapError (Debug.log "lambda decode error")

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen(\kind ->
      case kind of
        "lambdaEvent" ->
          Decode.map Videos (Decode.at ["event", "user_id"] Decode.string)
        _ -> Decode.fail kind
    )

test : Cmd msg
test =
  Encode.object
    [ ("kind", Encode.string "test")
    ]
    |> lambdaCommand

port lambdaCommand : Value -> Cmd msg
port lambdaEvent : (Value -> msg) -> Sub msg
