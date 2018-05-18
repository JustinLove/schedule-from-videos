module Twitch.Template exposing (imageTemplateUrl, imagePercentTemplateUrl)

imageTemplateUrl : Int -> Int -> String -> String
imageTemplateUrl w h =
  String.split "{width}"
    >> String.join (toString w)
    >> String.split "{height}"
    >> String.join (toString h)

imagePercentTemplateUrl : Int -> Int -> String -> String
imagePercentTemplateUrl w h =
  String.split "%{width}"
    >> String.join (toString w)
    >> String.split "%{height}"
    >> String.join (toString h)
