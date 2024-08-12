{- Author: Fabian Huch, TU München

Find Facts searcher component: Url-encoded 'dry' query state,
enriched by facet information from query.
-}
module Searcher exposing (Model, empty, params, parser, Msg, Update(..), update, populate,
  set_result, view, get_query)


import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Extra as Html
import Library exposing (..)
import Material.Chip.Filter as FilterChip
import Material.ChipSet.Filter as FilterChipSet
import Material.Elevation as Elevation
import Material.IconButton as IconButton
import Material.LayoutGrid as LayoutGrid
import Material.Select as Select
import Material.Select.Item as SelectItem
import Material.TextField as TextField
import Material.TextField.Icon as TextFieldIcon
import Material.Typography as Typography
import Parser exposing (Parser)
import Query exposing (Query)
import Set exposing (Set)
import Html.Lazy as Lazy
import Url.Builder exposing (QueryParameter)
import Maybe exposing (Maybe)
import Maybe.Extra as Maybe
import Utils exposing (Query_Param, parse_key)


{- config -}

max_facet_terms = 5


{- fields -}

sessionN = "session"
theoryN = "theory"
commandN = "command"
sourceN = "source"
nameN = "name"
constsN = "consts"
typsN = "typs"
thmsN = "thms"
kindsN = "kinds"

search_fields = [sessionN, theoryN, commandN, sourceN, nameN, constsN, typsN, thmsN]
facet_fields = [sessionN, theoryN, commandN, constsN, typsN, thmsN, kindsN]


{- search components -}

type alias Facet = {field: String, terms: Set String}
type alias Filter = {field: String, value: String, exclude: Bool}
type alias Search = {any_filter: String, filters: Array Filter, facets: Dict String Facet}
type Model = Model {search: Search, facets: Maybe Query.Facets}

init search = Model {search = search, facets = Nothing}

empty: Model
empty = init (Search "" Array.empty Dict.empty)


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

params: Model -> List QueryParameter
params (Model model) = search_params model.search

{- Url parsing -}

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

parser: Parser Query_Param Model
parser = Parser.map init search_parser


{- update -}

type Msg =
  Any_Input String |
  Add_Filter String |
  Filter_Input Int String |
  Change_Filter Int |
  Remove_Filter Int |
  Change_Facet String String

type Update = Empty Query | None Query | Later Query | Now Query

empty_filter field = Filter field "" False
update_filter value filter = {filter | value = value}
change_filter filter = {filter | exclude = (not filter.exclude)}
change_terms value terms = (if Set.member value terms then Set.remove else Set.insert) value terms

change_facet : String -> String -> Maybe Facet -> Maybe Facet
change_facet field value facet0 =
  let
    facet1 = Maybe.withDefault (Facet field Set.empty) facet0
    facet2 = {facet1 | terms = facet1.terms |> change_terms value}
  in if Set.isEmpty facet2.terms then Nothing else Just facet2

update: Msg -> Model -> (Model, Update)
update msg (Model model) =
  let
    search = model.search
    maybe_same h m = if Maybe.withDefault True m then None else h
    update_search search1 upd =
      let query1 = search_query search1
      in if Query.empty_query query1
        then (Model {model | search = search1, facets = Nothing}, Empty query1)
        else (Model {model | search = search1}, upd query1)
  in case msg of
    Any_Input value ->
      update_search {search | any_filter = value}
        (if value == search.any_filter then None else Later)
    Add_Filter field ->
     update_search {search | filters = search.filters |> Array.push (empty_filter field)} None
    Filter_Input i value ->
      update_search {search | filters = search.filters |> Array.update i (update_filter value)}
        (maybe_same Later (Array.get i search.filters |> Maybe.map (.value >> (==) value)))
    Change_Filter i ->
      update_search {search | filters = search.filters |> Array.update i change_filter}
        (maybe_same Now (Array.get i search.filters |> Maybe.map (.value >> String.isEmpty)))
    Remove_Filter i ->
      update_search {search | filters = search.filters |> Array.removeAt i}
        (maybe_same Now (Array.get i search.filters |> Maybe.map (.value >> String.isEmpty)))
    Change_Facet field value ->
      update_search
        {search | facets = search.facets |> Dict.update field (change_facet field value)} Now

populate: Model -> Model -> Model
populate (Model model0) (Model model) = Model {model0 | search = model.search}

set_result: Query.Result -> Model -> Model
set_result res (Model model) = Model {model | facets = Just res.facets}


{- view -}

view_field: String -> String
view_field field =
  Dict.fromList [(sessionN, "Session"), (theoryN, "Theory"), (commandN, "Command"),
    (sourceN, "Source"), (nameN, "Name"), (constsN, "Constant"), (typsN, "Type"),
    (thmsN, "Theorem"), (kindsN, "Kind")]
  |> Dict.get field
  |> Maybe.withDefault field

view_filter: (Int, Filter) -> Html Msg
view_filter (i, filter) =
  LayoutGrid.inner [Typography.body1, style "margin" "16px 0"] [
    LayoutGrid.cell
      [LayoutGrid.span3Phone, LayoutGrid.span7Tablet, LayoutGrid.span11Desktop,
        LayoutGrid.alignMiddle]
      [TextField.outlined
        (TextField.config
         |> TextField.setLeadingIcon (Just (
           TextFieldIcon.icon (if filter.exclude then "block" else "done")
           |> TextFieldIcon.setOnInteraction (Change_Filter i)))
         |> TextField.setValue (Just filter.value)
         |> TextField.setOnInput (Filter_Input i)
         |> TextField.setAttributes [style "width" "100%"]
         |> TextField.setLabel (Just filter.field))],
    LayoutGrid.cell [LayoutGrid.span1, LayoutGrid.alignLeft] [
      IconButton.iconButton
        (IconButton.config |> IconButton.setOnClick (Remove_Filter i))
        (IconButton.icon "close")]]

view_facet: String -> String -> List String -> Dict String Int -> Set String -> Html Msg
view_facet field t ts counts selected =
  let
    chip term =
      FilterChip.chip
        (FilterChip.config
         |> FilterChip.setSelected (Set.member term selected)
         |> FilterChip.setOnChange (Change_Facet field term))
        (term ++ maybe_proper (Dict.get term counts) (\i -> " (" ++ String.fromInt i ++ ")"))
  in LayoutGrid.inner [Typography.body1] [
       LayoutGrid.cell [LayoutGrid.span2, LayoutGrid.alignMiddle] [text (view_field field)],
       LayoutGrid.cell [LayoutGrid.span10]
         [FilterChipSet.chipSet [] (chip t) (ts |> List.map chip)]]

view_add_filter : Html Msg
view_add_filter =
  let option field = SelectItem.selectItem (SelectItem.config {value=field}) field
  in case search_fields of
    [] -> Html.nothing
    x :: xs ->
      Select.outlined
        (Select.config
         |> Select.setLabel (Just "Add Filter")
         |> Select.setAttributes [style "margin" "16px 0"]
         |> Select.setOnChange Add_Filter)
        (option x)
        (xs |> List.map option)

view: Model -> Html Msg
view (Model model) =
  let
    maybe_view_facet field =
      let
        counts =
          model.facets |> Maybe.map (Dict.get field) |> Maybe.join |> Maybe.withDefault Dict.empty
        selected =
          model.search.facets |> Dict.get field |> Maybe.map .terms |> Maybe.withDefault Set.empty
        counts1 = if Dict.size counts > max_facet_terms then Dict.empty else counts
        terms = Dict.keys counts1 |> Set.fromList |> Set.union selected |> Set.toList |> List.sort
      in case terms of
        [] -> Nothing
        t::ts -> Just (Lazy.lazy5 view_facet field t ts counts1 selected)
    facets = facet_fields |> List.filterMap maybe_view_facet
  in
    LayoutGrid.layoutGrid [Elevation.z2, style "margin" "16px 0"] ([
      TextField.outlined (TextField.config
      |> TextField.setPlaceholder (Just "Enter search terms...")
      |> TextField.setAttributes
        [style "width" "100%", style "background-color" "white", style "margin-bottom" "16px"]
      |> TextField.setValue (Just model.search.any_filter)
      |> TextField.setOnInput Any_Input),
      h3 [Typography.headline6] [text "Filters"]] ++ (
      Array.toIndexedList model.search.filters
      |> List.map (Lazy.lazy view_filter)) ++
    [view_add_filter] ++ (
    list_if (not (List.isEmpty facets)) (h3 [Typography.headline6] [text "Drill-down"])) ++
    facets)


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

get_query: Model -> Query.Query
get_query (Model model) = search_query model.search
