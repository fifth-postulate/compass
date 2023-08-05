module Automaton.Compass exposing (Compass(..), toString)


type Compass
    = North
    | East
    | South
    | West


toString : Compass -> String
toString heading =
    case heading of
        North ->
            "↑"

        East ->
            "→"

        South ->
            "↓"

        West ->
            "←"
