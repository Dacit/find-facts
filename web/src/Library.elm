{- Author: Fabian Huch, TU München

Basic library functions.
-}
module Library exposing (quote, try_unquote, perhaps_unquote, if_proper, get_msg)


import Http


quote: String -> String
quote s = "\"" ++ s ++ "\""

try_unquote: String -> Maybe String
try_unquote s =
  if String.startsWith "\"" s && String.endsWith "\"" s then Just (String.slice 1 -1 s) else Nothing

perhaps_unquote: String -> String
perhaps_unquote s = try_unquote s |> Maybe.withDefault s

if_proper: Bool -> a -> List a
if_proper cond x = if cond then [x] else []

get_msg: Http.Error -> String
get_msg error =
  let print_error = (++) "*** "
  in case error of
    Http.BadUrl s -> print_error "Internal Error: Malformed Url " ++ quote s
    Http.Timeout -> print_error "Timeout"
    Http.NetworkError -> print_error "Network Error"
    Http.BadStatus i -> print_error ("Status Code " ++ String.fromInt i)
    Http.BadBody s -> print_error "Internal Error: Bad Body" ++ quote s
