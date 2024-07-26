{- Author: Fabian Huch, TU München

Basic library functions.
-}
module Library exposing (quote, try_unquote, perhaps_unquote, if_proper)


quote: String -> String
quote s = "\"" ++ s ++ "\""

try_unquote: String -> Maybe String
try_unquote s =
  if String.startsWith "\"" s && String.endsWith "\"" s then Just (String.slice 1 -1 s) else Nothing

perhaps_unquote: String -> String
perhaps_unquote s = try_unquote s |> Maybe.withDefault s

if_proper: Bool -> a -> List a
if_proper cond x = if cond then [x] else []
