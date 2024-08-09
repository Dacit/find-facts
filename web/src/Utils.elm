{- Author: Fabian Huch, TU München

Various utilities.
-}
module Utils exposing (..)


import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Parser
import Html.Parser.Util
import Maybe.Extra as Maybe
import Parser exposing (Parser)
import String.Extra as String
import Library exposing (..)
import Url


{- query params -}

type alias Query_Param = (String, String)

parse_query: String -> Parser Query_Param a -> Maybe a
parse_query query parser =
  let
    pair s =
      case String.split "=" s |> List.map Url.percentDecode of
        [Just k, Just v] -> Just (k, v)
        _ -> Nothing
    params = if query == "" then [] else String.split "&" query
    pairs = params |> List.map pair |> Maybe.combine
  in pairs |> Maybe.map (Parser.parse parser) |> Maybe.join

parse_key: (a -> Bool) -> Parser (a, b) (a, b)
parse_key cond = Parser.elem (Tuple.first >> cond)


{- code blocks -}

escape_html: String -> String
escape_html s = s
  |> String.replace "&" "&amp;"
  |> String.replace "<" "&lt;"
  |> String.replace ">" "&gt;"
  |> String.replace "\"" "&quot;"

view_html: String -> Html msg
view_html html =
  case Html.Parser.run html of
    Ok nodes -> span [] (Html.Parser.Util.toVirtualDom nodes)
    _ -> text (String.stripTags html)

view_code: String -> Int -> Html msg
view_code src start =
  let
    lines = split_lines src
    end = start + (List.length lines) - 1
    numbers =
      List.range start end |> List.map String.fromInt |> List.intersperse "\n" |> String.concat
  in
    div [style "display" "inline-flex"] [
      Html.pre
        [style "float" "left", style "text-align" "right", style "min-width" "5ch",
          class "source", style "color" "gray"]
        [text numbers],
      Html.pre [style "float" "right", class "source"] [view_html src]
    ]