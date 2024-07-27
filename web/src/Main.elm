module Main exposing (init, main, subscriptions, update, view)


import Http
import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Query exposing (Query)
import Searcher
import Url exposing (Url)
import Url.Builder
import Utils


{- main -}

main: Program () Model Msg
main =
 Browser.application {init = init, view = view, update = update, subscriptions = subscriptions,
   onUrlChange = UrlChanged, onUrlRequest = LinkClicked}


{- model -}

type Page = Not_Found | About | Search Searcher.Model Query
type alias Model = {nav_key: Navigation.Key, url: Url, page: Page}

url_encode: Url -> Page -> Url
url_encode url page =
  case page of
    Not_Found -> {url | fragment = Nothing}
    About -> {url | fragment = Just "about"}
    Search model _ ->
      let
        params = Searcher.search_params model.search
      in {url | fragment = Just ("search" ++ (Url.Builder.toQuery params))}

fragment_decode: String -> Page
fragment_decode fragment =
  case String.split "?" fragment of
    ["about"] -> About
    "search" :: qs ->
      Utils.parse_query (String.join "?" qs) Searcher.search_parser
      |> Maybe.map (\search -> Search (Searcher.init search) (Searcher.search_query search))
      |> Maybe.withDefault Not_Found
    _ -> Not_Found

url_decode: Url -> Page
url_decode url = url.fragment |> Maybe.map fragment_decode |> Maybe.withDefault Not_Found

init: () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
  case url.fragment of
    Just fragment -> (Model key url (fragment_decode fragment), Cmd.none)
    Nothing ->
      let
        search = Searcher.empty
        page = Search (Searcher.init search) (Searcher.search_query search)
      in (Model key url page, page |> url_encode url |> push_url key)


{- commands -}

push_url: Navigation.Key -> Url -> Cmd msg
push_url key url = Navigation.pushUrl key (Url.toString url)

get_result: Url -> Query -> Cmd Msg
get_result url query =
  Http.request {method = "GET", headers = [], timeout = Nothing, tracker = Nothing,
    url = Url.toString {url | path = url.path ++ "../api/query", fragment = Nothing},
    expect = Http.expectJson Query_Result Query.decode_result,
    body = query |> Query.encode_query |> Http.jsonBody}


{- update -}

type Msg =
  LinkClicked Browser.UrlRequest |
  UrlChanged Url |
  Searcher Searcher.Msg |
  Query_Result (Result Http.Error Query.Result)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> (model, Navigation.pushUrl model.nav_key (Url.toString url))
        Browser.External href -> (model, Navigation.load href)
    UrlChanged url -> ({model | url = url, page = url_decode url}, Cmd.none)
    Searcher msg1 ->
      case model.page of
        Search search query ->
          let
            (model1, commit) = Searcher.update msg1 search
            query1 = Searcher.search_query model1.search
            page = Search model1 query1
            search_cmd =
              if commit then url_encode model.url page |> push_url model.nav_key else Cmd.none
            query_cmd =
              if query1 /= query then get_result model.url query else Cmd.none
          in ({model | page = page}, Cmd.batch [search_cmd, query_cmd])
        _ -> (model, Cmd.none)
    Query_Result result -> (model, Cmd.none)


{- subscriptions -}

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none


{- view -}

view: Model -> Browser.Document Msg
view model =
  case model.page of
    Not_Found -> {title = "404 Not Found", body = [text "404 Not Found"]}
    About -> {title = "Find Facts | About", body = [text ""]}
    Search search _ ->
      {title = "Find Facts | Searcher", body = [Searcher.view search |> Html.map Searcher]}
