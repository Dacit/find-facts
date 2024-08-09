{- Author: Fabian Huch, TU München

Find Facts results component.
-}
module Results exposing (Model, empty, Msg(..), set_loading, set_error, set_result, set_load_more,
  set_loaded, get_maybe_cursor, view)


import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Extra as Html
import Html.Lazy as Lazy
import Http
import Material.Card as Card
import Material.Elevation as Elevation
import Material.LayoutGrid as LayoutGrid
import Material.Typography as Typography
import Material.LinearProgress as LinearProgress
import Query exposing (Block)
import Library exposing (..)
import Utils


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
  Card.card (Card.setAttributes [Elevation.z3, style "margin-bottom" "16px"] Card.config |> Card.setOnClick (Selected block.id))
    {actions = Nothing,
     blocks = (
       Card.block
         (LayoutGrid.layoutGrid [LayoutGrid.alignLeft, style "width" "100%"] [
           div [Typography.caption, style "margin-bottom" "8px"] [text block.theory],
           Utils.view_code block.html block.start_line]),
       [])}

view_results blocks loading =
  let
    num = List.length blocks.blocks
    loaded = span [Typography.subtitle1] [text ("Loaded " ++ (String.fromInt num) ++ "/" ++
      (String.fromInt blocks.num_found) ++ if_proper (blocks.num_found > num) ". Scroll for more ...")]
  in div [] (
    h2 [Typography.headline4] [text ("Found " ++ String.fromInt blocks.num_found ++ " results")] ::
    (blocks.blocks |> List.map (Lazy.lazy view_block)) ++
    [if loading then LinearProgress.indeterminate LinearProgress.config else loaded])

view: Model -> Html Msg
view model =
  case model of
    Empty -> Html.nothing
    Loading -> LinearProgress.indeterminate LinearProgress.config
    Error msg -> span [Typography.body1] [text msg]
    Results blocks loading -> Lazy.lazy2 view_results blocks loading
