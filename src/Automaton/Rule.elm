module Automaton.Rule exposing (Rule, action, lookup, rule)

import Automaton.Action exposing (Action)
import Automaton.Cell as Cell exposing (CellType, Surrounding)


type Rule a
    = Rule
        { a
            | north : CellType
            , east : CellType
            , south : CellType
            , west : CellType
            , action : Action
        }


rule : CellType -> CellType -> CellType -> CellType -> Action -> Rule {}
rule north east south west anAction =
    Rule { north = north, east = east, south = south, west = west, action = anAction }


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
match surrounding (Rule aRule) =
    (Cell.north surrounding == aRule.north)
        && (Cell.east surrounding == aRule.east)
        && (Cell.south surrounding == aRule.south)
        && (Cell.west surrounding == aRule.west)


action : Rule a -> Action
action (Rule aRule) =
    aRule.action
