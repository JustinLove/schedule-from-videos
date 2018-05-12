module View exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type Msg
  = None

css = """
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , text "view"
    ]
