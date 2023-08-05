module Automaton.Rule exposing (Rule, rule)

import Automaton.Action exposing (Action)
import Automaton.Cell exposing (CellType)


type alias Rule a =
    { a
        | north : CellType
        , east : CellType
        , south : CellType
        , west : CellType
        , action : Action
    }


rule : CellType -> CellType -> CellType -> CellType -> Action -> Rule {}
rule north east south west anAction =
    { north = north, east = east, south = south, west = west, action = anAction }
