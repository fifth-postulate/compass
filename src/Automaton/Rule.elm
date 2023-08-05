module Automaton.Rule exposing (Rule, lookup, rule)

import Automaton.Action exposing (Action)
import Automaton.Cell exposing (CellType, Surrounding)


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


lookup : Surrounding -> List (Rule a) -> Maybe (Rule a)
lookup surrounding rules =
    case rules of
        [] ->
            Nothing

        aRule :: tail ->
            if match surrounding aRule then
                Just aRule

            else
                lookup surrounding tail


match : Surrounding -> Rule a -> Bool
match surrounding aRule =
    (surrounding.north == aRule.north)
        && (surrounding.east == aRule.east)
        && (surrounding.south == aRule.south)
        && (surrounding.west == aRule.west)
