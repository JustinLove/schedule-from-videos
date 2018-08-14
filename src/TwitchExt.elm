port module TwitchExt exposing (..)

import Json.Decode exposing (..)

port onAuthorized : (Auth -> msg) -> Sub msg
port onContext : (Context -> msg) -> Sub msg
port onError : (Json.Decode.Value -> msg) -> Sub msg

type alias Auth =
  { channelId : String
  , clientId : String
  , token : String
  , userId : String
  }

auth : Decoder Auth
auth =
  map4 Auth
    (field "channelId" string)
    (field "clientId" string)
    (field "token" string)
    (field "userId" string)

type alias Context =
  { theme : String
  , mode : String
  }

context : Decoder Context
context =
  map2 Context
    (field "theme" string)
    (field "mode" string)
