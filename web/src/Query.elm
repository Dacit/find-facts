{- Author: Fabian Huch, TU Muenchen

Queries against the find facts server.
-}
module Query exposing (Facets)


import Dict exposing (Dict)


type alias Facets = Dict String (Dict String Int)
