{- Author: Fabian Huch, TU München

Search results. -}

module Results exposing (Model, init, Msg, update, set_loading, set_results, set_error, view)


import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Query exposing (Block)
import Library exposing (..)


{- model -}

type alias Model = {get_blocks: String -> (Result Http.Error Query.Blocks -> Msg) -> Cmd Msg, state: State}
type State = Empty | Loading | Error String | Results Query.Blocks Bool

init: (String -> (Result Http.Error Query.Blocks -> Msg) -> Cmd Msg) -> Model
init get_blocks = Model get_blocks Empty


{- update -}

type Msg = Load_More | Loaded (Result Http.Error Query.Blocks)

update: Model -> Msg -> (Model, Cmd Msg)
update model msg =
  case model.state of
    Results blocks _ ->
      case msg of
        Load_More -> ({model | state = Results blocks True}, model.get_blocks blocks.cursor Loaded)
        Loaded (Result.Ok blocks1) ->
          let blocks2 = {blocks1 | blocks = blocks.blocks ++ blocks1.blocks}
          in ({model | state = Results blocks2 False}, Cmd.none)
        Loaded (Result.Err error) -> ({model | state = Error (get_msg error)}, Cmd.none)
    _ -> (model, Cmd.none)

set_loading: Model -> Model
set_loading model = {model | state = Loading}

set_results: Query.Result -> Model ->  Model
set_results res model = {model | state = Results res.blocks False }

set_error: Http.Error -> Model -> Model
set_error err model = {model | state = Error (get_msg err)}


{- view -}

view_block block =
  p [] [
    text block.theory,
    a [href block.url] [],
    text block.html]

view: Model -> Html Msg
view model =
  case model.state of
    Empty -> div [] []
    Loading -> text "Loading..."
    Error msg -> text msg
    Results blocks loading ->
      div [] (
        text ("Found " ++ String.fromInt blocks.num_found) ::
        (blocks.blocks |> List.map view_block) ++
        if_proper loading (text "Loading..."))
