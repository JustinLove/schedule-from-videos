module Twitch exposing (helix, twitchHeaders, authHeaders)

import Http
import Json.Decode

helix :
  { clientId : String
  , auth : Maybe String
  , decoder : Json.Decode.Decoder a
  , tagger : ((Result Http.Error a) -> msg)
  , url : String
  } -> Cmd msg
helix {clientId, auth, decoder, tagger, url} =
  Http.send tagger <| Http.request
    { method = "GET"
    , headers = twitchHeaders clientId auth
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

twitchHeaders : String -> Maybe String -> List Http.Header
twitchHeaders clientId auth =
  List.append
    [ Http.header "Client-ID" clientId
    ] (authHeaders auth)

authHeaders : Maybe String -> List Http.Header
authHeaders auth =
  case auth of
    Just token ->
      [ Http.header "Authorization" ("Bearer "++token) ]
    Nothing ->
      []
