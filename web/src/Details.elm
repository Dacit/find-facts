{- Author: Fabian Huch, TU München

Find Facts details view.
-}
module Details exposing (Model, init, get_id, params, parser, set_loaded, view)


import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Extra as Html exposing (viewIf)
import Http
import Library exposing (..)
import Material.Theme as Theme
import Material.Typography as Typography
import Parser exposing (Parser)
import Query exposing (Block)
import Url.Builder as Builder exposing (QueryParameter)
import Utils exposing (Query_Param, parse_key)


{- model -}

type State = Loading | Error String | Loaded Block
type Model = Model {id: String, state: State}

init id = Model {id = id, state = Loading}

get_id: Model -> String
get_id (Model model) = model.id

params: Model -> List QueryParameter
params (Model model) = [Builder.string "id" model.id]

parser: Parser Query_Param Model
parser = Parser.map (Tuple.second >> init) (parse_key ((==) "id"))


{- updates -}

set_loaded: Result Http.Error Block -> Model -> Model
set_loaded res (Model model) =
  case res of
    Result.Ok block -> Model {model | state = Loaded block}
    Result.Err err -> Model {model | state = Error (get_msg err)}


{- view -}

view: Model -> Html Never
view (Model model) =
  case model.state of
    Loading -> text "Loading ..."
    Error msg -> text msg
    Loaded block ->
      let
        start_before =
          block.start_line - count_lines (block.src_before ++ block.html) + count_lines block.html
        around s = "<span style=\"color: gray\">" ++ (Utils.escape_html s) ++ "</span>"
        code = around block.src_before ++ block.html ++ around block.src_after
        view_counts name elems =
          if List.isEmpty elems then Html.nothing
          else
            span
              [style "margin-right" "8px", style "padding" "4px 8px", style "border-radius" "16px",
                Theme.secondaryBg, Theme.onSecondary]
                [text (name ++ ": " ++ String.fromInt (List.length elems))]
      in div [] [
        h2 [Typography.headline4] [text "Details"],
        h3 [Typography.headline6] [text "Theory ", a [href block.url] [text block.theory]],
        Utils.view_code code start_before,
        viewIf (block.consts ++ block.typs ++ block.thms /= []) (h3 [Typography.subtitle1] [
          view_counts "Constants" block.consts,
          view_counts "Types" block.typs,
          view_counts "Theorems" block.thms])]
