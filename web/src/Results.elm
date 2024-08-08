{- Author: Fabian Huch, TU München

Search results. -}

module Results exposing (Model, empty, Msg(..), set_loading, set_error, set_result, set_load_more,
  set_loaded, get_maybe_cursor, view)


import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Lazy as Lazy
import Http
import Query exposing (Block)
import Library exposing (..)
import Utils exposing (view_html)


{- model -}

type Model = Empty | Loading | Error String | Results Query.Blocks Bool

empty: Model
empty = Empty


{- updates -}

type Msg = Selected String

set_loading: Model
set_loading = Loading

set_error: Http.Error -> Model
set_error err = Error (get_msg err)

set_result: Query.Result -> Model
set_result res = Results res.blocks False

set_load_more: Model -> Model
set_load_more model =
  case model of
    Results blocks _ -> Results blocks True
    _ -> model

set_loaded: Result Http.Error Query.Blocks -> Model -> Model
set_loaded result model =
  case (result, model) of
    (Result.Ok blocks1, Results blocks _) ->
      let blocks2 = {blocks1 | blocks = blocks.blocks ++ blocks1.blocks}
      in Results blocks2 False
    (Result.Err err, _) -> Error (get_msg err)
    _ -> model

get_maybe_cursor: Model -> Maybe String
get_maybe_cursor model =
  case model of
    Results blocks loading ->
      if blocks.num_found <= List.length blocks.blocks || loading then Nothing
      else Just blocks.cursor
    _ -> Nothing


{- view -}

view_block block =
  span [] [p [] [text block.theory], pre [onClick (Selected block.id)] [view_html block.html]]

view_results blocks loading =
  let
    num = List.length blocks.blocks
    loaded_text = "Loaded " ++ (String.fromInt num) ++ "/" ++ (String.fromInt blocks.num_found) ++
      if_proper (blocks.num_found > num) ". Scroll for more ..."
  in div [style "height" "100%"] (
    text ("Found " ++ String.fromInt blocks.num_found ++ " results") ::
    (blocks.blocks |> List.map (Lazy.lazy view_block) |> List.intersperse (br [] [])) ++
    [text (if loading then "Loading..." else loaded_text)])

view: Model -> Html Msg
view model =
  case model of
    Empty -> Html.nothing
    Loading -> text "Loading..."
    Error msg -> text msg
    Results blocks loading -> Lazy.lazy2 view_results blocks loading
