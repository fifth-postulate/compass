module Automaton.Location exposing (Location, go)

import Automaton.Compass exposing (Compass(..))


type alias Location =
    ( Int, Int )


go : Compass -> Location -> Location
go direction ( x, y ) =
    case direction of
        North ->
            ( x, y - 1 )

        East ->
            ( x + 1, y )

        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )
