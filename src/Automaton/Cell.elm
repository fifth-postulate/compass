module Automaton.Cell exposing (CellType(..), Surrounding)


type CellType
    = Occupied
    | Free


type alias Surrounding =
    { north : CellType
    , east : CellType
    , south : CellType
    , west : CellType
    }
