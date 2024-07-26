module Main exposing (init, main, subscriptions, update, view)


import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
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

type Page = Not_Found | About | Search Searcher.Model
type alias Model = {nav_key: Navigation.Key, url: Url, page: Page}

url_encode url page =
  case page of
    Not_Found -> {url | fragment = Nothing}
    About -> {url | fragment = Just "about"}
    Search model ->
      let
        params = Searcher.search_params model.search
      in {url | fragment = Just ("search" ++ (Url.Builder.toQuery params))}

fragment_decode fragment =
  case String.split "?" fragment of
    ["about"] -> About
    "search" :: qs ->
      Utils.parse_query (String.join "?" qs) Searcher.search_parser
      |> Maybe.map (Searcher.init >> Search)
      |> Maybe.withDefault Not_Found
    _ -> Not_Found

url_decode url = url.fragment |> Maybe.map fragment_decode |> Maybe.withDefault Not_Found

pushUrl key url = Navigation.pushUrl key (Url.toString url)

init: () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
  case url.fragment of
    Just fragment -> (Model key url (fragment_decode fragment), Cmd.none)
    Nothing ->
      let
        page = Searcher.empty |> Searcher.init |> Search
      in (Model key url page, page |> url_encode url |> pushUrl key)


{- update -}

type Msg = LinkClicked Browser.UrlRequest | UrlChanged Url | Searcher Searcher.Msg

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
        Search search ->
          let
            (search1, commit) = Searcher.update msg1 search
            page = Search search1
            cmd = if commit then url_encode model.url page |> pushUrl model.nav_key else Cmd.none
          in ({model | page = page}, cmd)
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
    Search search ->
      {title = "Find Facts | Searcher", body = [Searcher.view search |> Html.map Searcher]}
