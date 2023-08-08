module Automaton.Surrounding exposing (CellType(..), Surrounding, fromInt, surrounding, view)

import Css
    exposing
        ( Px
        , borderBottomWidth
        , borderBox
        , borderColor
        , borderLeftWidth
        , borderRightWidth
        , borderStyle
        , borderTopWidth
        , boxSizing
        , display
        , height
        , hex
        , inlineBlock
        , px
        , solid
        , width
        )
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute


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
surrounding north east south west =
    Surrounding
        { north = north
        , east = east
        , south = south
        , west = west
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

        north : CellType
        north =
            m
                |> modBy 2
                |> toStatus

        east : CellType
        east =
            (m // 2)
                |> modBy 2
                |> toStatus

        south : CellType
        south =
            (m // 4)
                |> modBy 2
                |> toStatus

        west : CellType
        west =
            (m // 8)
                |> modBy 2
                |> toStatus
    in
    surrounding north east south west


view : Surrounding -> Html msg
view (Surrounding s) =
    let
        toWidth : CellType -> Px
        toWidth status =
            case status of
                Free ->
                    px 1

                Occupied ->
                    px 3
    in
    Html.span
        [ Attribute.css
            [ display inlineBlock
            , width <| px 15
            , height <| px 15
            , boxSizing borderBox
            , borderStyle solid
            , borderColor <| hex "000000"
            , borderTopWidth <| toWidth <| s.north
            , borderRightWidth <| toWidth <| s.east
            , borderBottomWidth <| toWidth <| s.south
            , borderLeftWidth <| toWidth <| s.west
            ]
        ]
        []
