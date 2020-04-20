module Env exposing (Env(..), PlainEnv, EncryptedEnv, env, decode)

import Json.Decode exposing (..)

type alias PlainEnv =
  { clientId : String
  , clientSecret : String
  }

type alias EncryptedEnv =
  { clientId : String
  , clientSecret : String
  }

type Env
  = Plain PlainEnv
  | Encrypted PlainEnv

decode : Value -> Result Error Env
decode = decodeValue env

env : Decoder Env
env =
  oneOf
    [ map Plain plainEnv
    , map Encrypted encryptedEnv
    ]

plainEnv : Decoder PlainEnv
plainEnv =
  map2 PlainEnv
    (field "TWITCH_CLIENT_ID" string)
    (field "TWITCH_CLIENT_SECRET" string)

encryptedEnv : Decoder EncryptedEnv
encryptedEnv =
  map2 PlainEnv
    (field "TWITCH_CLIENT_ID_ENCRYPTED" string)
    (field "TWITCH_CLIENT_SECRET_ENCRYPTED" string)
