module Automaton exposing (Automaton, create, step, view)

import Automaton.Action as Action exposing (Action)
import Automaton.Compass exposing (Compass)
import Automaton.Program as Program exposing (Program)
import Automaton.Rule as Rule
import Automaton.State exposing (State)
import Automaton.Surrounding exposing (Surrounding)
import Html.Styled exposing (Html)


create : State -> Program -> Automaton
create start program =
    Automaton
        { currentState = start
        , program = program
        }


type Automaton
    = Automaton
        { currentState : State
        , program : Program
        }


step : Surrounding -> Automaton -> Maybe ( Automaton, Compass )
step surrounding ((Automaton { currentState, program }) as automat) =
    program
        |> Program.rules currentState
        |> Maybe.andThen (Rule.lookup surrounding)
        |> Maybe.map Rule.action
        |> Maybe.map (apply automat)


apply : Automaton -> Action -> ( Automaton, Compass )
apply (Automaton automaton) action =
    ( Automaton { automaton | currentState = Action.nextState action }, Action.heading action )


view : Automaton -> Html msg
view (Automaton { currentState, program }) =
    Program.view currentState program
