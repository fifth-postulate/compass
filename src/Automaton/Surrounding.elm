module Automaton.Surrounding exposing (CellType(..), Surrounding, east, fromInt, north, south, surrounding, west)


type Surrounding
    = Surrounding
        { north : CellType
        , east : CellType
        , south : CellType
        , west : CellType
        }


type CellType
    = Occupied
    | Free


surrounding : CellType -> CellType -> CellType -> CellType -> Surrounding
surrounding n e s w =
    Surrounding
        { north = n
        , east = e
        , south = s
        , west = w
        }


fromInt : Int -> Surrounding
fromInt m =
    let
        toStatus : Int -> CellType
        toStatus d =
            case d of
                0 ->
                    Free

                _ ->
                    Occupied

        n : CellType
        n =
            m
                |> modBy 2
                |> toStatus

        e : CellType
        e =
            (m // 2)
                |> modBy 2
                |> toStatus

        s : CellType
        s =
            (m // 4)
                |> modBy 2
                |> toStatus

        w : CellType
        w =
            (m // 8)
                |> modBy 2
                |> toStatus
    in
    surrounding n e s w


north : Surrounding -> CellType
north (Surrounding s) =
    s.north


east : Surrounding -> CellType
east (Surrounding s) =
    s.east


south : Surrounding -> CellType
south (Surrounding s) =
    s.south


west : Surrounding -> CellType
west (Surrounding s) =
    s.west
