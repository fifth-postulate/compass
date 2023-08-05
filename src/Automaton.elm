module Automaton exposing (Automaton, automaton, step, view)

import Automaton.Action exposing (Action)
import Automaton.Cell exposing (Surrounding)
import Automaton.Compass exposing (Compass)
import Automaton.Program as Program exposing (Program)
import Automaton.Rule as Rule
import Automaton.State exposing (State)
import Html.Styled exposing (Html)


automaton : State -> Program a -> Automaton a
automaton start program =
    Automaton
        { currentState = start
        , program = program
        }


type Automaton a
    = Automaton (Data a)


type alias Data a =
    { currentState : State
    , program : Program a
    }


step : Surrounding -> Automaton a -> Maybe ( Automaton a, Compass )
step surrounding ((Automaton { currentState, program }) as automat) =
    program
        |> Program.rules currentState
        |> Maybe.andThen (Rule.lookup surrounding)
        |> Maybe.map Rule.action
        |> Maybe.map (apply automat)


apply : Automaton a -> Action -> ( Automaton a, Compass )
apply (Automaton data) { nextState, heading } =
    ( Automaton { data | currentState = nextState }, heading )


view : Automaton a -> Html msg
view (Automaton { currentState, program }) =
    Program.view currentState program
