module Details exposing (Model, init, get_id, params, parser, set_loaded, view)


import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Library exposing (..)
import Parser exposing (Parser)
import Query exposing (Block)
import Url.Builder as Builder exposing (QueryParameter)
import Utils exposing (Query_Param, parse_key, view_html)


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
        view_name prefix name = a [href (block.url ++ "#" ++ name ++ prefix)] [text name]
        view_elems kind prefix names =
          if List.isEmpty names then []
          else [
            h3 [] [text kind],
            ul [] (names |> List.map ((view_name prefix) >> List.singleton >> li []))]
      in div [] ([
        h2 [] [text "Theory ", a [href block.url] [text block.theory]],
        pre [] [text block.src_before],
        view_html block.html,
        pre [] [text block.src_after]] ++
        view_elems "Constants" "|const" block.consts ++
        view_elems "Types" "|type" block.typs ++
        view_elems "Theorems" "|thm" block.thms)
