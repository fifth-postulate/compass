module Maze.Cell exposing (Cell(..), fromString, isBarrier, isEmpty, isMachine, toCellType)

import Automaton.Surrounding exposing (CellType(..))


type Cell
    = Barrier
    | Machine
    | Empty


fromString : String -> Cell
fromString representation =
    case representation of
        "." ->
            Empty

        "@" ->
            Machine

        _ ->
            Barrier


isBarrier : Cell -> Bool
isBarrier cell =
    case cell of
        Barrier ->
            True

        _ ->
            False


isMachine : Cell -> Bool
isMachine cell =
    case cell of
        Machine ->
            True

        _ ->
            False


isEmpty : Cell -> Bool
isEmpty cell =
    case cell of
        Empty ->
            True

        _ ->
            False


toCellType : Cell -> CellType
toCellType cell =
    case cell of
        Empty ->
            Free

        _ ->
            Occupied
