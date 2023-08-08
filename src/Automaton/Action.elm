module Automaton.Action exposing (Action, action, heading, nextState)

import Automaton.Compass exposing (Compass)
import Automaton.State exposing (State)


type Action
    = Action
        { nextState : State
        , heading : Compass
        }


action : State -> Compass -> Action
action s h =
    Action { nextState = s, heading = h }


nextState : Action -> State
nextState (Action a) =
    a.nextState


heading : Action -> Compass
heading (Action a) =
    a.heading
