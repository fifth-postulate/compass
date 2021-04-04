module Automaton exposing (Automaton, Compass(..), Rule, Situation, State, Status(..), action, automaton, rule, step)

import Dict exposing (Dict)


automaton : State -> Dict State (List (Rule a)) -> Automaton a
automaton start table =
    Automaton
        { start = start
        , current = start
        , table = table
        }


type Automaton a
    = Automaton (Data a)


type alias Data a =
    { start : State
    , current : State
    , table : Dict State (List (Rule a))
    }


type alias State =
    Int


type alias Rule a =
    { a
        | north : Status
        , east : Status
        , south : Status
        , west : Status
        , action : Action
    }


rule : Status -> Status -> Status -> Status -> Action -> Rule {}
rule north east south west anAction =
    { north = north, east = east, south = south, west = west, action = anAction }


type Status
    = Occupied
    | Free


type alias Action =
    { nextState : State
    , direction : Compass
    }


action : State -> Compass -> Action
action nextState direction =
    { nextState = nextState, direction = direction }


type Compass
    = North
    | East
    | South
    | West


type alias Situation =
    { north : Status
    , east : Status
    , south : Status
    , west : Status
    }


step : Situation -> Automaton a -> Maybe ( Automaton a, Compass )
step situation ((Automaton { current, table }) as automat) =
    table
        |> Dict.get current
        |> Maybe.andThen (lookup situation)
        |> Maybe.map .action
        |> Maybe.map (flip apply <| automat)


lookup : Situation -> List (Rule a) -> Maybe (Rule a)
lookup situation rules =
    case rules of
        [] ->
            Nothing

        aRule :: tail ->
            if match situation aRule then
                Just aRule

            else
                lookup situation tail


match : Situation -> Rule a -> Bool
match situation aRule =
    (situation.north == aRule.north)
        && (situation.east == aRule.east)
        && (situation.south == aRule.south)
        && (situation.west == aRule.west)


apply : Action -> Automaton a -> ( Automaton a, Compass )
apply { nextState, direction } (Automaton data) =
    ( Automaton { data | current = nextState }, direction )


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
