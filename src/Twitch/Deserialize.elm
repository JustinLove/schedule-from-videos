module Twitch.Deserialize exposing
  ( Token
  , User
  , LiveStream
  , Game
  , Follow
  , Video
  , token
  , users
  , liveStreams
  , games
  , follows
  , videos
  )

import Json.Decode exposing (..)

{- sub = "12345678", iss = "https://api.twitch.tv/api", aud = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", exp = 1511110246, iat = 1511109346 -}

type alias Token =
  { sub : String
  , iss : String
  , aud : String
  , exp : Int
  , iat : Int
  }

token : Decoder Token
token =
  map5 Token
    (field "sub" string)
    (field "iss" string)
    (field "aud" string)
    (field "exp" int)
    (field "iat" int)

{-"data":[{
   "id":"44322889",
   "login":"dallas",
   "display_name":"dallas",
   "type":"staff",
   "broadcaster_type":"",
   "description":"Just a gamer playing games and chatting. :)",
   "profile_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-profile_image-1a2c906ee2c35f12-300x300.png",
   "offline_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-channel_offline_image-1a2c906ee2c35f12-1920x1080.png",
   "view_count":191836881,
   "email":"login@provider.com"
}]-}


type alias User =
  { id : String
  , displayName : String
  }

users : Decoder (List User)
users =
  field "data" (list user)

user : Decoder User
user =
  map2 User
    (field "id" string)
    (field "display_name" string)

{-"data":
   [
      {
         "id":"26007494656",
         "user_id":"23161357",
         "game_id":"417752",
         "community_ids":[
            "5181e78f-2280-42a6-873d-758e25a7c313",
            "848d95be-90b3-44a5-b143-6e373754c382",
            "fd0eab99-832a-4d7e-8cc0-04d73deb2e54"
         ],
         "type":"live",
         "title":"Hey Guys, It's Monday - Twitter: @Lirik",
         "viewer_count":32575,
         "started_at":"2017-08-14T16:08:32Z",
         "language":"en",
         "thumbnail_url":"https://static-cdn.jtvnw.net/previews-ttv/live_user_lirik-{width}x{height}.jpg"
      }, 
      ... 
   ], 
   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjp7Ik9mZnNldCI6MjB9fQ=="}
-}

type alias LiveStream =
  { channelId : String
  , userId : String
  , gameId : String
  , title : String
  , viewerCount : Int
  , thumbnailUrl : String
  }

liveStreams : Decoder (List LiveStream)
liveStreams =
  field "data" (list stream)

stream : Decoder LiveStream
stream =
  map6 LiveStream
    (field "id" string)
    (field "user_id" string)
    (field "game_id" string)
    (field "title" string)
    (field "viewer_count" int)
    (field "thumbnail_url" string)

{-"data":
   [
      {
         "id":"493057",
         "name":"PLAYERUNKNOWN'S BATTLEGROUNDS",
         "box_art_url":"https://static-cdn.jtvnw.net/ttv-boxart/PLAYERUNKNOWN%27S%20BATTLEGROUNDS-{width}x{height}.jpg"
      }
   ]
-}

type alias Game =
  { id : String
  , name : String
  , boxArtUrl : String
  }

games : Decoder (List Game)
games =
  field "data" (list game)

game : Decoder Game
game =
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "box_art_url" string)

{-"data":
   [
      {
         "from_id":"171003792",
         "to_id":"23161357",
         "followed_at":"2017-08-22T22:55:24Z"
      },
      {
         "from_id":"113627897",
         "to_id":"23161357",
         "followed_at":"2017-08-22T22:55:04Z"
      },
      . . . 
   ],
   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9"}
-}

type alias Follow =
  { from_id : String
  , to_id : String
  }

follows : Decoder (List Follow)
follows =
  field "data" (list follow)

follow : Decoder Follow
follow =
  map2 Follow
    (field "from_id" string)
    (field "to_id" string)

{-
   "data":
      [
         {
            "id":"172982667",
            "user_id":"141981764",
            "title":"Developer Demonstrations - Twitch Extensions",
            "description":"See a demonstration of Twitch Extensions from Curse, Muxy, OP.GG, Overwolf, Proletariat, and Streamlabs.",
            "created_at":"2017-09-07T15:20:56Z",
            "published_at":"2017-09-18T15:12:45Z",
            "thumbnail_url":"https://static-cdn.jtvnw.net/s3_vods/twitchdev/172982667/942dca70-f00a-4ace-bac1-ca41eea0b524/thumb/custom93d2276919a54213-%{width}x%{height}.png",
            "view_count":307,
            "language":"en"
         }
      ]
-}

type alias Video =
  { id : String
  , userId : String
  , title : String
  , createdAt : String
  , publishedAt : String
  , thumbnailUrl : String
  }

videos : Decoder (List Video)
videos =
  field "data" (list video)

video : Decoder Video
video =
  map6 Video
    (field "id" string)
    (field "user_id" string)
    (field "title" string)
    (field "created_at" string)
    (field "published_at" string)
    (field "thumbnail_url" string)
