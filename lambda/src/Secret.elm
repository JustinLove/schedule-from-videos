module Secret exposing (Secret, fromString, toString)

import Json.Decode exposing (Value)
import Json.Encode

type Secret = Secret Value

fromString : String -> Secret
fromString = Secret << Json.Encode.string

toString : Secret -> String
toString (Secret v) =
  v
    |> Json.Decode.decodeValue Json.Decode.string
    |> Result.withDefault ""
