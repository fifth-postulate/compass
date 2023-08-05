module Automaton.Action exposing (Action, action)

import Automaton.Compass exposing (Compass)
import Automaton.State exposing (State)


type alias Action =
    { nextState : State
    , heading : Compass
    }


action : State -> Compass -> Action
action nextState heading =
    { nextState = nextState, heading = heading }
