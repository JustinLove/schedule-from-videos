port module MeasureText exposing (TextSpec, TextSize, getTextWidth, textSize)

type alias TextSpec =
  { font : String
  , text : String
  }

type alias TextSize =
  { text : String
  , width : Float
  }

port getTextWidth : TextSpec -> Cmd msg
port textSize : (TextSize -> msg) -> Sub msg
