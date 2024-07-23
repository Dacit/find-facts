module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view}

type Msg = Unit

init _ = ()

update msg model =
    case msg of
        Unit -> model

view model =
    div [] []