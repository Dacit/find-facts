module Main exposing (init, main, subscriptions, update, view)


import Http
import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Library exposing (..)
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

type Page = Not_Found | About | Search Searcher.Model Query Results.Model
type alias Model = {nav_key: Navigation.Key, url: Url, page: Page}

should_query : Maybe Query -> Query -> Bool
should_query query query1 = query /= Just query1 && not (Query.empty_query query1)

update_search: Page -> Url -> Searcher.Search -> Page
update_search previous url search =
  case previous of
    Search searcher query results ->
      let
        query1 = Searcher.search_query search
        results1 = if should_query (Just query) query1 then Results.set_loading results else results
      in Search {searcher | search = search} query1 results1
    _ ->
     let
       query = Searcher.search_query search
       results0 = Results.init (get_blocks query)
       results = if should_query Nothing query then Results.set_loading results0 else results0
     in Search (Searcher.init search) query results

init: () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  case url.fragment of
    Just fragment ->
      let
        page = fragment_decode (update_search Not_Found url) fragment
        cmd =
          case page of
            Search _ query _ ->
              if should_query Nothing query then get_result query else Cmd.none
            _ -> Cmd.none
      in (Model key url page, cmd)
    Nothing ->
      let
        search = Searcher.empty
        page = Search (Searcher.init search) (Searcher.search_query search)
          (Results.init (get_blocks {filters = []}))
      in (Model key url page, page |> url_encode url |> push_url key)


{- url coding -}

url_encode: Url -> Page -> Url
url_encode url page =
  case page of
    Not_Found -> {url | fragment = Nothing}
    About -> {url | fragment = Just "about"}
    Search model _ _ ->
      let
        params = Searcher.search_params model.search
      in {url | fragment = Just ("search" ++ (Url.Builder.toQuery params))}

fragment_decode : (Searcher.Search -> Page) -> String -> Page
fragment_decode make_search1 fragment =
  case String.split "?" fragment of
    ["about"] -> About
    "search" :: qs ->
      Utils.parse_query (String.join "?" qs) Searcher.search_parser
      |> Maybe.map make_search1
      |> Maybe.withDefault Not_Found
    _ -> Not_Found

url_decode: Url -> Page -> Page
url_decode url page =
  case url.fragment of
    Nothing -> Not_Found
    Just fragment -> fragment_decode (update_search page url) fragment


{- commands -}

push_url: Navigation.Key -> Url -> Cmd msg
push_url key url = Navigation.replaceUrl key (Url.toString url)

get_result: Query -> Cmd Msg
get_result query =
  Http.post {url="/api/query", expect = Http.expectJson (Query_Result query) Query.decode_result,
    body= query |> Query.encode_query |> Http.jsonBody}

get_blocks : Query -> String -> (Result Http.Error Query.Blocks -> Results.Msg) -> Cmd Results.Msg
get_blocks query cursor msg =
  Http.post {url = "/api/blocks", expect = Http.expectJson msg Query.decode_blocks, body =
    {query = query, cursor = cursor} |> Query.encode_query_blocks |> Http.jsonBody}


{- update -}

type Msg =
  Link_Clicked Browser.UrlRequest |
  Url_Changed Url |
  Searcher Searcher.Msg |
  Results Results.Msg |
  Query_Result Query (Result Http.Error Query.Result)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Link_Clicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> (model, Navigation.pushUrl model.nav_key (Url.toString url))
        Browser.External href -> (model, Navigation.load href)
    Url_Changed url ->
      let
        page = url_decode url model.page
        query =
          case model.page of
            Search _ query1 _ -> Just query1
            _ -> Nothing
        cmd =
          case page of
            Search _ query1 _ ->
              if should_query query query1 then get_result query1 else Cmd.none
            _ -> Cmd.none
      in ({model | url = url, page = page}, cmd)
    Searcher msg1 ->
      case model.page of
        Search search _ results ->
          let
            model1 = Searcher.update msg1 search
            query1 = Searcher.search_query model1.search
            page = Search model1 query1 results
            cmd = url_encode model.url page |> push_url model.nav_key
          in ({model | page = page}, cmd)
        _ -> (model, Cmd.none)
    Results msg1 ->
      case model.page of
        Search search query results ->
          let
            (results1, cmd) = Results.update results msg1
          in ({model | page = Search search query results1}, Cmd.map Results cmd)
        _ -> (model, Cmd.none)
    Query_Result query result ->
      case model.page of
        Search search query1 results ->
          case result of
            Result.Ok res ->
              if query /= query1 then (model, Cmd.none)
              else
               let
                 search1 = Searcher.set_results res search
                 results1 = Results.set_results res results
              in ({model | page = Search search1 query1 results1}, Cmd.none)
            Result.Err err -> ({model | page = Search search query (Results.set_error err results)}, Cmd.none)

        _ -> (model, Cmd.none)


{- subscriptions -}

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none


{- view -}

view: Model -> Browser.Document Msg
view model =
  case model.page of
    Not_Found -> {title = "404 Not Found", body = [text "404 Not Found"]}
    About -> {title = "Find Facts | About", body = [text ""]}
    Search search _ results ->
      {title = "Find Facts | Searcher", body =
        [Searcher.view search |> Html.map Searcher, Results.view results |> Html.map Results]}
