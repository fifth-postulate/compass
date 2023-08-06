module Automaton.Rule exposing (Rule, action, lookup, rule)

import Automaton.Action exposing (Action)
import Automaton.Cell exposing (CellType, Surrounding, surrounding)


type Rule
    = Rule
        { prerequisite : Surrounding
        , action : Action
        }


rule : CellType -> CellType -> CellType -> CellType -> Action -> Rule
rule north east south west anAction =
    Rule { prerequisite = surrounding north east south west, action = anAction }


lookup : Surrounding -> List Rule -> Maybe Rule
lookup surrounding rules =
    case rules of
        [] ->
            Nothing

        aRule :: tail ->
            if match surrounding aRule then
                Just aRule

            else
                lookup surrounding tail


match : Surrounding -> Rule -> Bool
match surrounding (Rule aRule) =
    surrounding == aRule.prerequisite


action : Rule -> Action
action (Rule aRule) =
    aRule.action
