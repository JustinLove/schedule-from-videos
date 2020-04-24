module Env exposing (Env(..), PlainEnv, EncryptedEnv, env, decode)

import Secret exposing (Secret)

import Json.Decode exposing (..)

type alias PlainEnv =
  { clientId : Secret
  , clientSecret : Secret
  }

type alias EncryptedEnv =
  { clientId : Secret
  , clientSecret : Secret
  }

type Env
  = Plain PlainEnv
  | Encrypted EncryptedEnv

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
    (field "TWITCH_CLIENT_ID" secret)
    (field "TWITCH_CLIENT_SECRET" secret)

encryptedEnv : Decoder EncryptedEnv
encryptedEnv =
  map2 PlainEnv
    (field "TWITCH_CLIENT_ID_ENCRYPTED" secret)
    (field "TWITCH_CLIENT_SECRET_ENCRYPTED" secret)

secret : Decoder Secret
secret =
  map Secret.fromString string
