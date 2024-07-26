{- Author: Fabian Huch, TU München

Utilities.
-}
module Utils exposing (..)


import Maybe.Extra as Maybe
import Parser exposing (Parser)
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
