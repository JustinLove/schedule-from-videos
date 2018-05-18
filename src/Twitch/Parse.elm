module Twitch.Parse exposing (duration, int)

import Parser exposing (..)
import Char
import Time exposing (Time)

duration : Parser Time
duration =
  succeed (\h m s -> toFloat (60 * 60 * h + 60 * m + s) * 1000)
    |= (suffixedInt "h" |> withDefault 0)
    |= (suffixedInt "m" |> withDefault 0)
    |= (suffixedInt "s")
    |. end

withDefault : a -> Parser a -> Parser a
withDefault default parser =
  oneOf
    [ parser
    , succeed default
    ]

suffixedInt : String -> Parser Int
suffixedInt suffix =
  inContext ("int suffixed with " ++ suffix) <|
    delayedCommitMap (\i _ -> i) int (symbol suffix)

int : Parser Int
int =
  inContext "int" <|
    (keep oneOrMore Char.isDigit
      |> andThen (\s -> case String.toInt s of
        Ok i -> succeed i
        Err err -> fail err
      )
    )

