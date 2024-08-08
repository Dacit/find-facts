{- Author: Fabian Huch, TU München

Utilities.
-}
module Utils exposing (..)


import Html exposing (..)
import Html.Parser
import Html.Parser.Util
import Maybe.Extra as Maybe
import Parser exposing (Parser)
import String.Extra as String
import Url


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

parse_key : (a -> Bool) -> Parser (a, b) (a, b)
parse_key cond = Parser.elem (Tuple.first >> cond)

view_html : String -> Html msg
view_html html =
  case Html.Parser.run html of
    Ok nodes -> span [] (Html.Parser.Util.toVirtualDom nodes)
    _ -> text (String.stripTags html)