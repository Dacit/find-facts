{- Author: Fabian Huch, TU München

Simple delayed events.
-}
module Delay exposing (Model, empty, Msg, Delay, invoke, update)


import Dict exposing (Dict)
import Process
import Task


{- model to count invocations -}

type Model = Model (Dict String Int)

empty: Model
empty = Model Dict.empty


{- invoking delays -}

type Msg msg = Msg (Delay msg)
type alias Delay msg = {name: String, delay: Float, event: Cmd msg}

invoke: Model -> Delay msg -> (Model, Cmd (Msg msg))
invoke (Model model) delay =
  let
    num = model |> Dict.get delay.name |> Maybe.withDefault 0
    model1 = model |> Dict.insert delay.name (num + 1)
    cmd = Task.perform (always (Msg delay)) (Process.sleep delay.delay)
  in (Model model1, cmd)


{- update -}

update: Model -> (Msg msg) -> (Model, Cmd msg)
update (Model model) (Msg delay) =
  let num = model |> Dict.get delay.name |> Maybe.withDefault 0
  in if num > 1
    then (model |> Dict.insert delay.name (num - 1) |> Model, Cmd.none)
    else (model |> Dict.remove delay.name |> Model, delay.event)
