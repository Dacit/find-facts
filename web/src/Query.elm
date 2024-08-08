{- Author: Fabian Huch, TU Muenchen

Queries against the find facts server.
-}
module Query exposing (..)


import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode


{- queries -}

type Atom = Value String | Phrase String | Wildcard String
type Term = Or (List Atom) | Not Atom
type Filter = Any_Filter Term | Field_Filter String Term
type alias Query = {filters: List Filter}
type alias Query_Blocks = {query: Query, cursor: String}

empty_atom: Atom -> Bool
empty_atom atom =
  case atom of
    Value s -> String.words s |> List.isEmpty
    Phrase s -> String.words s |> List.isEmpty
    Wildcard s -> String.words s |> List.isEmpty

empty_term: Term -> Bool
empty_term term =
  case term of
    Or atoms -> atoms |> List.all empty_atom
    Not atom ->  empty_atom atom

empty_filter: Filter -> Bool
empty_filter filter =
  case filter of
    Any_Filter term -> empty_term term
    Field_Filter _ term -> empty_term term

empty_query: Query -> Bool
empty_query query = query.filters |> List.all empty_filter


{- json encoding -}

encode_atom: Atom -> Encode.Value
encode_atom atom =
  case atom of
    Value value -> Encode.object [("value", Encode.string value)]
    Phrase phrase -> Encode.object [("phrase", Encode.string phrase)]
    Wildcard wildcard -> Encode.object [("wildcard", Encode.string wildcard)]

encode_term: Term -> Encode.Value
encode_term term =
  case term of
    Or atoms -> Encode.object [("in", Encode.list encode_atom atoms)]
    Not atom -> Encode.object [("not", encode_atom atom)]

encode_filter: Filter -> Encode.Value
encode_filter filter =
  case filter of
    Any_Filter term -> Encode.object [("term", encode_term term)]
    Field_Filter field term ->
      Encode.object [("term", encode_term term), ("field", Encode.string field)]

encode_query: Query -> Encode.Value
encode_query query =
  Encode.object [("filters", Encode.list encode_filter query.filters)]

encode_query_blocks: Query_Blocks -> Encode.Value
encode_query_blocks query_blocks =
  Encode.object
    [("query", encode_query query_blocks.query), ("cursor", Encode.string query_blocks.cursor)]


{- results -}

type alias Block = {
  id: String,
  session: String,
  theory: String,
  url: String,
  command: String,
  start_line: Int,
  src_before: String,
  src_after: String,
  html: String,
  consts: List String,
  typs: List String,
  thms: List String}

type alias Blocks = {num_found: Int, blocks: List Block, cursor: String}
type alias Facets = Dict String (Dict String Int)
type alias Result = {blocks: Blocks, facets: Facets}


{- json decoding -}

decode_block: Decode.Decoder Block
decode_block =
  Decode.succeed Block
  |> Decode.andMap (Decode.field "id" Decode.string)
  |> Decode.andMap (Decode.field "session" Decode.string)
  |> Decode.andMap (Decode.field "theory" Decode.string)
  |> Decode.andMap (Decode.field "url" Decode.string)
  |> Decode.andMap (Decode.field "command" Decode.string)
  |> Decode.andMap (Decode.field "start_line" Decode.int)
  |> Decode.andMap (Decode.field "src_before" Decode.string)
  |> Decode.andMap (Decode.field "src_after" Decode.string)
  |> Decode.andMap (Decode.field "html" Decode.string)
  |> Decode.andMap (Decode.field "consts" (Decode.list Decode.string))
  |> Decode.andMap (Decode.field "typs" (Decode.list Decode.string))
  |> Decode.andMap (Decode.field "thms" (Decode.list Decode.string))

decode_blocks: Decode.Decoder Blocks
decode_blocks =
  Decode.map3 Blocks
    (Decode.field "num_found" Decode.int)
    (Decode.field "blocks" (Decode.list decode_block))
    (Decode.field "cursor" Decode.string)

decode_facets: Decode.Decoder Facets
decode_facets = Decode.dict (Decode.dict Decode.int)

decode_result: Decode.Decoder Result
decode_result =
  Decode.map2 Result (Decode.field "blocks" decode_blocks) (Decode.field "facets" decode_facets)
