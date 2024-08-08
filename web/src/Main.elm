module Main exposing (init, main, subscriptions, update, view)


import Delay
import Details
import Html.Attributes exposing (style)
import Html.Events as Events
import Http
import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Json.Decode as Decode
import Query exposing (Query, Query_Blocks)
import Results
import Searcher
import Url exposing (Url)
import Url.Builder
import Utils


{- main -}

main: Program () Model Msg
main =
 Browser.application {init = init, view = view, update = update, subscriptions = subscriptions,
   onUrlChange = Url_Changed, onUrlRequest = Link_Clicked}


{- model -}

type Page = Not_Found | About | Detail Details.Model | Search Searcher.Model Query Results.Model
type alias Model = {nav_key: Navigation.Key, url: Url, page: Page, delay: Delay.Model}

should_query : Maybe Query -> Query -> Bool
should_query query query1 = query /= Just query1 && not (Query.empty_query query1)

update_search: Page -> Searcher.Search -> Page
update_search previous search =
  case previous of
    Search searcher query results ->
      let
        query1 = Searcher.search_query search
        results1 =
          if should_query (Just query) query1 then Results.set_loading
          else if Query.empty_query query1 then Results.empty
          else results
      in Search {searcher | search = search} query1 results1
    _ ->
     let
       query = Searcher.search_query search
       results0 = Results.empty
       results = if should_query Nothing query then Results.set_loading else results0
     in Search (Searcher.init search) query results

init: () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  case url.fragment of
    Just fragment ->
      let
        page = fragment_decode (update_search Not_Found) fragment
        search_cmd =
          case page of
            Search _ query _ ->
              if should_query Nothing query then get_result query else Cmd.none
            _ -> Cmd.none
      in (Model key url page Delay.empty, search_cmd)
    Nothing ->
      let
        search = Searcher.empty
        page = Search (Searcher.init search) (Searcher.search_query search) Results.empty
        url_cmd = page |> url_encode url |> push_url key
      in (Model key url page Delay.empty, url_cmd)


{- url encoding/decoding -}

url_encode: Url -> Page -> Url
url_encode url page =
  case page of
    Not_Found -> {url | fragment = Nothing}
    About -> {url | fragment = Just "about"}
    Detail details ->
      {url | fragment = Just ("details" ++ Url.Builder.toQuery (Details.params details))}
    Search searcher _ _ ->
      let params = Searcher.search_params searcher.search
      in {url | fragment = Just ("search" ++ (Url.Builder.toQuery params))}

fragment_decode : (Searcher.Search -> Page) -> String -> Page
fragment_decode make_search1 fragment =
  case String.split "?" fragment of
    ["about"] -> About
    "details" :: qs ->
      Utils.parse_query (String.join "?" qs) Details.parser
      |> Maybe.map Detail
      |> Maybe.withDefault Not_Found
    "search" :: qs ->
      Utils.parse_query (String.join "?" qs) Searcher.search_parser
      |> Maybe.map make_search1
      |> Maybe.withDefault Not_Found
    _ -> Not_Found

url_decode: Url -> Page -> Page
url_decode url page =
  case url.fragment of
    Nothing -> Not_Found
    Just fragment -> fragment_decode (update_search page) fragment


{- commands -}

push_url: Navigation.Key -> Url -> Cmd msg
push_url key url = Navigation.replaceUrl key (Url.toString url)

get_result: Query -> Cmd Msg
get_result query =
  Http.post {url="/api/query", expect = Http.expectJson (Query_Result query) Query.decode_result,
    body = query |> Query.encode_query |> Http.jsonBody}

get_blocks: Query -> String -> Cmd Msg
get_blocks query cursor =
  Http.post {url = "/api/blocks", expect = Http.expectJson (Query_Blocks query) Query.decode_blocks,
    body = {query = query, cursor = cursor} |> Query.encode_query_blocks |> Http.jsonBody}

get_block: String -> Cmd Msg
get_block id =
  Http.post {url = "/api/block", expect = Http.expectJson (Query_Block id) Query.decode_block,
    body = id |> Query.encode_query_block |> Http.jsonBody}


{- update -}

type alias Scroll_Info = {pos: Float, top: Float, height: Float}
type Msg =
  Link_Clicked Browser.UrlRequest |
  Url_Changed Url |
  Searcher Searcher.Msg |
  Delay (Delay.Msg Msg) |
  Results (Results.Msg) |
  Query_Result Query (Result Http.Error Query.Result) |
  Query_Blocks Query (Result Http.Error Query.Blocks) |
  Query_Block String (Result Http.Error Query.Block) |
  Scroll_Event Scroll_Info

query_delay: Query -> Delay.Delay Msg
query_delay query = {name = "query", delay = 500, event = get_result query}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Link_Clicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> (model, Navigation.pushUrl model.nav_key (Url.toString url))
        Browser.External href -> (model, Navigation.load href)

    Url_Changed url ->
      let
        query0 =
          case model.page of
            Search _ query1 _ -> Just query1
            _ -> Nothing
        page = url_decode url model.page
        (delay, cmd) =
          case page of
            Search _ query _ ->
              if should_query query0 query
                then Delay.invoke model.delay (query_delay query) |> Tuple.mapSecond (Cmd.map Delay)
                else (model.delay, Cmd.none)
            Detail details -> (model.delay, get_block (Details.get_id details))
            _ -> (model.delay, Cmd.none)
      in ({model | url = url, page = page, delay = delay}, cmd)

    Delay msg1 ->
      let (delay, cmd) = Delay.update model.delay msg1
      in ({model | delay = delay}, cmd)

    Searcher msg1 ->
      case model.page of
        Search search _ results ->
          let
            model1 = Searcher.update msg1 search
            query1 = Searcher.search_query model1.search
            page = Search model1 query1 results
            cmd = url_encode model.url page |> push_url model.nav_key
          in (model, cmd)
        _ -> (model, Cmd.none)

    Results (Results.Selected id) ->
      (model, url_encode model.url (id |> Details.init |> Detail) |> push_url model.nav_key)

    Query_Result query res ->
      case model.page of
        Search search query1 _ ->
          case res of
            Result.Ok result ->
              if query /= query1 then (model, Cmd.none)
              else
                let
                  search1 = Searcher.set_result result search
                  results = Results.set_result result
                in ({model | page = Search search1 query1 results}, Cmd.none)
            Result.Err err ->
              ({model | page = Search search query (Results.set_error err)}, Cmd.none)
        _ -> (model, Cmd.none)

    Query_Blocks query res ->
      case model.page of
        Search search query1 results ->
          if query /= query1 then (model, Cmd.none)
          else ({model | page = Search search query (Results.set_loaded res results)}, Cmd.none)
        _ -> (model, Cmd.none)

    Query_Block id res ->
      case model.page of
        Detail details ->
          if id /= (Details.get_id details) then (model, Cmd.none)
          else ({model | page = Detail (Details.set_loaded res details)}, Cmd.none)
        _ -> (model, Cmd.none)

    Scroll_Event scroll ->
      if (scroll.pos - scroll.top) > scroll.height then (model, Cmd.none)
      else
        case model.page of
          Search search query results ->
            case Results.get_maybe_cursor results of
              Nothing -> (model, Cmd.none)
              Just cursor ->
                let results1 = Results.set_load_more results
                in ({model | page = Search search query results1}, get_blocks query cursor)
          _ -> (model, Cmd.none)



{- subscriptions -}

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none


{- view -}

decode_scroll : Decode.Decoder Msg
decode_scroll =
  Decode.map Scroll_Event (
    Decode.map3 Scroll_Info
      (Decode.at ["target", "scrollHeight"] Decode.float)
      (Decode.at ["target", "scrollTop"] Decode.float)
      (Decode.at ["target", "offsetHeight"] Decode.float))

view: Model -> Browser.Document Msg
view model =
  case model.page of
    Not_Found -> {title = "404 Not Found", body = [text "404 Not Found"]}
    About -> {title = "Find Facts | About", body = [text ""]}
    Detail details ->
      {title = "Find Facts | Details", body = [Details.view details |> Html.map never]}
    Search search _ results -> {
      title = "Find Facts | Search",
      body = [
        div [Events.on "scroll" decode_scroll, style "height" "100%", style "overflow" "scroll"]
          [Searcher.view search |> Html.map Searcher, Results.view results |> Html.map Results]]}
