module Automaton exposing (Automaton, automaton, step, view)

import Automaton.Action exposing (Action)
import Automaton.Cell exposing (CellType(..), Surrounding)
import Automaton.Compass as Compass exposing (Compass)
import Automaton.Rule as Rule exposing (Rule)
import Automaton.State exposing (State)
import Css
    exposing
        ( Px
        , Style
        , backgroundColor
        , borderBottomWidth
        , borderBox
        , borderCollapse
        , borderColor
        , borderLeftWidth
        , borderRightWidth
        , borderStyle
        , borderTopWidth
        , boxSizing
        , collapse
        , display
        , height
        , hex
        , inlineBlock
        , px
        , solid
        , width
        )
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute


automaton : State -> Dict State (List (Rule a)) -> Automaton a
automaton start table =
    Automaton
        { current = start
        , table = table
        }


type Automaton a
    = Automaton (Data a)


type alias Data a =
    { current : State
    , table : Dict State (List (Rule a))
    }


step : Surrounding -> Automaton a -> Maybe ( Automaton a, Compass )
step surrounding ((Automaton { current, table }) as automat) =
    table
        |> Dict.get current
        |> Maybe.andThen (Rule.lookup surrounding)
        |> Maybe.map Rule.action
        |> Maybe.map (apply automat)


apply : Automaton a -> Action -> ( Automaton a, Compass )
apply (Automaton data) { nextState, heading } =
    ( Automaton { data | current = nextState }, heading )


view : Automaton a -> Html msg
view (Automaton { current, table }) =
    viewTable current table


viewTable : State -> Dict State (List (Rule a)) -> Html msg
viewTable current table =
    let
        states : List State
        states =
            table
                |> Dict.keys
                |> List.sort

        rows : List (Html msg)
        rows =
            states
                |> List.map (\state -> ( state, Dict.get state table |> Maybe.withDefault [] ))
                |> List.map (uncurry (viewState current))

        surrounding : List (Html msg)
        surrounding =
            List.range 0 15
                |> List.map surroundingFromInt
                |> List.map viewSurroundingHeader
    in
    Html.table [ Attribute.css [ borderCollapse collapse ] ]
        [ Html.thead []
            [ Html.tr [] <| Html.td [] [] :: surrounding
            ]
        , Html.tbody []
            rows
        ]


viewSurroundingHeader : Surrounding -> Html msg
viewSurroundingHeader { north, east, south, west } =
    let
        toWidth : CellType -> Px
        toWidth status =
            case status of
                Free ->
                    px 1

                Occupied ->
                    px 3
    in
    Html.td []
        [ Html.span
            [ Attribute.css
                [ display inlineBlock
                , width <| px 15
                , height <| px 15
                , boxSizing borderBox
                , borderStyle solid
                , borderColor <| hex "000000"
                , borderTopWidth <| toWidth north
                , borderRightWidth <| toWidth east
                , borderBottomWidth <| toWidth south
                , borderLeftWidth <| toWidth west
                ]
            ]
            []
        ]


viewState : State -> State -> List (Rule a) -> Html msg
viewState current state rules =
    let
        surroundings : List (Html msg)
        surroundings =
            List.range 0 15
                |> List.map surroundingFromInt
                |> List.map (flip Rule.lookup rules)
                |> List.map (Maybe.map viewAction)
                |> List.map (Maybe.withDefault <| Html.td [] [])

        bgc : List Style
        bgc =
            if current == state then
                [ backgroundColor <| hex "FFF5EE" ]

            else
                []
    in
    Html.tr [ Attribute.css <| bgc ] <|
        Html.td [] [ Html.text <| String.fromInt state ]
            :: surroundings


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


viewAction : Rule a -> Html msg
viewAction aRule =
    let
        next : String
        next =
            aRule.action.nextState
                |> String.fromInt

        heading : String
        heading =
            Compass.toString aRule.action.heading
    in
    Html.td [] [ Html.text <| next ++ heading ]


surroundingFromInt : Int -> Surrounding
surroundingFromInt n =
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
            n
                |> modBy 2
                |> toStatus

        east : CellType
        east =
            (n // 2)
                |> modBy 2
                |> toStatus

        south : CellType
        south =
            (n // 4)
                |> modBy 2
                |> toStatus

        west : CellType
        west =
            (n // 8)
                |> modBy 2
                |> toStatus
    in
    { north = north, east = east, south = south, west = west }


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
