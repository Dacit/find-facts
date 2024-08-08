{- Author: Fabian Huch, TU München

Searcher component: Url-encoded 'dry' query state enriched by facet information from query.
-}
module Searcher exposing (Search, Model, empty, init, search_params, search_parser, Msg, update,
 set_result, view, search_query)


import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (placeholder, selected, value)
import Html.Events exposing (onClick, onInput)
import Library exposing (..)
import Parser exposing (Parser)
import Query
import Set exposing (Set)
import Url.Builder exposing (QueryParameter)
import Maybe exposing (Maybe)
import Maybe.Extra as Maybe
import Utils exposing (Query_Param)


{- config -}

max_facet_terms = 5
max_search_facet_terms = 20


{- fields -}

sessionN = "session"
theoryN = "theory"
commandN = "command"
sourceN = "source"
nameN = "name"
constantN = "consts"
typeN = "typs"
theoremN = "thms"
kindN = "kinds"

search_fields = [sessionN, theoryN, commandN, sourceN, nameN, constantN, typeN, theoremN]
search_facet_fields = [sessionN, theoryN, commandN, constantN, typeN, theoremN]
facet_fields = [sessionN, theoryN, commandN, constantN, typeN, theoremN, kindN]


{- search components -}

type alias Facet = {field: String, terms: Set String}
type alias Filter = {field: String, value: String, exclude: Bool}
type alias Search = {any_filter: String, filters: Array Filter, facets: Dict String Facet}
type alias Model = {search: Search, add_filter: Bool, facets: Maybe Query.Facets}

empty: Search
empty = Search "" Array.empty Dict.empty

init: Search -> Model
init search = Model search False Nothing


{- URL encoding -}

filter_params : Filter -> List QueryParameter
filter_params filter =
  [Url.Builder.string ((if filter.exclude then "-" else "!") ++ filter.field) filter.value ]

facet_params : Facet -> List QueryParameter
facet_params facet =
  facet.terms |> Set.toList |> List.sort |> List.map (Url.Builder.string facet.field)

search_params: Search -> List QueryParameter
search_params search =
  list_if (search.any_filter /= "") (Url.Builder.string "q" search.any_filter) ++
    (search.filters |> Array.toList |> List.concatMap filter_params) ++
    (search.facets |> Dict.values |> List.sortBy .field |> List.concatMap facet_params)


{- Url parsing -}

parse_key cond = Parser.elem (Tuple.first >> cond)

filter_parser: Parser Query_Param Filter
filter_parser =
  let
    make_filter exclude (key, value) = Filter (String.dropLeft 1 key) value exclude
    include_parser = parse_key (String.startsWith "!") |> Parser.map (make_filter False)
    exclude_parser = parse_key (String.startsWith "-") |> Parser.map (make_filter True)
  in Parser.or include_parser exclude_parser

facet_parser: Parser Query_Param Facet
facet_parser =
  let
    rest_parser (key, value) =
      Parser.rep (parse_key ((==) key))
      |> Parser.map (List.map Tuple.second >> ((::) value) >> Set.fromList >> Facet key)
  in Parser.elem (always True) |> Parser.flat_map rest_parser

search_parser: Parser Query_Param Search
search_parser =
  Parser.map3 Search
    (Parser.or (parse_key ((==) "q") |> Parser.map Tuple.second) (Parser.succeed ""))
    (Parser.rep filter_parser |> Parser.map Array.fromList)
    (Parser.rep facet_parser |> Parser.map
      (List.map (\facet -> (facet.field, facet)) >> Dict.fromList))


{- update -}

type Msg =
  Any_Input String |
  Open_Add_Filter |
  Add_Filter String |
  Filter_Input Int String |
  Change_Filter Int |
  Remove_Filter Int |
  Select_Facet String String Bool

empty_filter field = Filter field "" False
update_filter value filter = {filter | value = value}
change_filter filter = {filter | exclude = (not filter.exclude)}

update_facet : String -> String -> Bool -> Maybe Facet -> Maybe Facet
update_facet field value selected facet0 =
  let
    facet1 = Maybe.withDefault (Facet field Set.empty) facet0
    facet2 = {facet1 | terms = facet1.terms |> (if selected then Set.insert else Set.remove) value}
  in if Set.isEmpty facet2.terms then Nothing else Just facet2

update: Msg -> Model -> Model
update msg model =
  let
    search = model.search
    search1 =
      case msg of
        Any_Input value -> {search | any_filter = value}
        Add_Filter field ->
         {search | filters = search.filters |> Array.push (empty_filter field)}
        Filter_Input i value ->
          {search | filters = search.filters |> Array.update i (update_filter value)}
        Change_Filter i -> {search | filters = search.filters |> Array.update i change_filter}
        Remove_Filter i -> {search | filters = search.filters |> Array.removeAt i}
        Select_Facet field value selected ->
          {search | facets = search.facets |> Dict.update field (update_facet field value selected)}
        _ -> search
  in case msg of
    Open_Add_Filter -> {model | search = search1, add_filter = True}
    _ -> {model | search = search1}

set_result: Query.Result -> Model -> Model
set_result res model = {model | facets = Just res.facets}


{- view -}

view_filter_select i counts =
  let
    view_option (name, count) =
      option [value name] [text (name ++ " (" ++ String.fromInt count ++ ")")]
  in select [onInput (Filter_Input i)] (option [selected True] [text "Select"] ::
    (counts |> Dict.toList |> List.sortBy Tuple.first |> List.map view_option))

view_filter counts (i, filter) =
  let
    counts1 = if Dict.size counts > max_search_facet_terms then Dict.empty else counts
    select = List.member filter.field search_facet_fields && not (Dict.isEmpty counts1)
  in fieldset [] ([
    legend [] [text <| filter.field],
    button [onClick (Change_Filter i) ] [text (if filter.exclude then "not" else "in")]] ++
    list_if select (view_filter_select i counts1) ++ [
    input [placeholder "search ...", value filter.value, onInput (Filter_Input i)] [],
    button [onClick (Remove_Filter i)] [text "x"]])

view_term field count enabled term =
  let term_string = term ++ maybe_proper count (\i -> " (" ++ String.fromInt i ++ ")")
  in button [onClick (Select_Facet field term (not enabled))] [text term_string]

view_facet field counts selected =
  let
    counts1 = if (Dict.size counts > max_facet_terms) then Dict.empty else counts
    terms = Dict.keys counts1 |> Set.fromList |> Set.union selected |> Set.toList |> List.sort
    view_term1 term = view_term field (Dict.get term counts1) (Set.member term selected) term
  in
    if List.isEmpty terms then Nothing
    else Just (div [] (text field :: (terms |> List.map view_term1)))

view_add_filter =
  select [onInput Add_Filter] (
    option [selected True] [text "Add filter"] ::
    (search_fields |> List.map (\field -> option [value field] [text field])))

view: Model -> Html Msg
view model =
  let
    get_counts field =
      model.facets |> Maybe.map (Dict.get field) |> Maybe.join |> Maybe.withDefault Dict.empty
    get_selected field =
      model.search.facets |> Dict.get field |> Maybe.map .terms |> Maybe.withDefault Set.empty
    view_facet1 field = view_facet field (get_counts field) (get_selected field)
  in form [] [
    p [] [
      input [placeholder "search anywhere ...", value model.search.any_filter, onInput Any_Input]
        [],
      p [] (
        text "Filters" :: (
        model.search.filters
        |> Array.toIndexedList
        |> List.map (\(i, filter) -> view_filter (get_counts filter.field) (i, filter))
        |> List.intersperse (hr [] [])) ++
        [view_add_filter]),
      p [] (text "Drill-down" ::
        (facet_fields |> List.filterMap view_facet1 |> List.intersperse (hr [] [])))]]


{- queries -}

explode_query: String -> Query.Atom
explode_query s =
 case try_unquote s of
   Just s1 -> Query.Phrase s1
   Nothing ->
    (if String.contains "*" s || String.contains "?" s then Query.Wildcard else Query.Value) s

term_query: String -> Query.Term
term_query s = explode_query s |> List.singleton |> Query.Or

filter_query: Filter -> Query.Filter
filter_query filter =
  Query.Field_Filter filter.field (filter.value |>
    (if filter.exclude then (explode_query >> Query.Not) else term_query))

facet_query: Facet -> Query.Filter
facet_query facet =
  Query.Field_Filter facet.field (Set.toList facet.terms |> List.map Query.Phrase |> Query.Or)

search_query: Search -> Query.Query
search_query search =
  Query.Query (
    (list_if (search.any_filter /= "") (search.any_filter |> term_query |> Query.Any_Filter)) ++
    (Array.toList search.filters |> List.map filter_query) ++
    (Dict.toList search.facets |> List.map Tuple.second |> List.map facet_query))
