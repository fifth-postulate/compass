module Automaton exposing (Automaton, Compass(..), Rule, Situation, State, Status(..), action, automaton, rule, step, view)

import Dict exposing (Dict)
import Html exposing (Html, table)


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


type alias State =
    Int


type alias Rule a =
    { a
        | north : Status
        , east : Status
        , south : Status
        , west : Status
        , action : Action
    }


rule : Status -> Status -> Status -> Status -> Action -> Rule {}
rule north east south west anAction =
    { north = north, east = east, south = south, west = west, action = anAction }


type Status
    = Occupied
    | Free


type alias Action =
    { nextState : State
    , direction : Compass
    }


action : State -> Compass -> Action
action nextState direction =
    { nextState = nextState, direction = direction }


type Compass
    = North
    | East
    | South
    | West


type alias Situation =
    { north : Status
    , east : Status
    , south : Status
    , west : Status
    }


step : Situation -> Automaton a -> Maybe ( Automaton a, Compass )
step situation ((Automaton { current, table }) as automat) =
    table
        |> Dict.get current
        |> Maybe.andThen (lookup situation)
        |> Maybe.map .action
        |> Maybe.map (flip apply <| automat)


lookup : Situation -> List (Rule a) -> Maybe (Rule a)
lookup situation rules =
    case rules of
        [] ->
            Nothing

        aRule :: tail ->
            if match situation aRule then
                Just aRule

            else
                lookup situation tail


match : Situation -> Rule a -> Bool
match situation aRule =
    (situation.north == aRule.north)
        && (situation.east == aRule.east)
        && (situation.south == aRule.south)
        && (situation.west == aRule.west)


apply : Action -> Automaton a -> ( Automaton a, Compass )
apply { nextState, direction } (Automaton data) =
    ( Automaton { data | current = nextState }, direction )


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


view : Automaton a -> Html msg
view (Automaton { current, table }) =
    Html.div []
        [ viewCurrentState current
        , viewTable table
        ]


viewCurrentState : Int -> Html msg
viewCurrentState current =
    Html.span [] [ Html.text <| (++) "Current state: " <| String.fromInt current ]


viewTable : Dict State (List (Rule a)) -> Html msg
viewTable table =
    let
        states =
            table
                |> Dict.keys
                |> List.sort

        rows =
            states
                |> List.map (viewState table)
        situations =
            List.range 0 15
                |> List.map situationFromInt
                |> List.map viewSituationHeader
     in
    Html.table []
        [ Html.thead []
            [ Html.tr [] <| (Html.td [] [ Html.text "State" ]) :: situations
            ]
        , Html.tbody []
            rows
        ]


viewSituationHeader : Situation -> Html msg
viewSituationHeader {north, east, south, west} =
    Html.td [] [Html.span [] [ Html.text "p"]]

viewState : Dict State (List (Rule a)) -> State -> Html msg
viewState table state =
    let
        situations =
            List.range 0 15
                |> List.map situationFromInt
    in
    Html.tr []
        [ Html.td [] [ Html.text <| String.fromInt state ]
        ]


situationFromInt : Int -> Situation
situationFromInt n =
    let
        toStatus d =
            case d of
                0 ->
                    Free

                _ ->
                    Occupied

        north =
            n
                |> modBy 2
                |> toStatus

        east =
            (n // 2)
                |> modBy 2
                |> toStatus

        south =
            (n // 4)
                |> modBy 2
                |> toStatus

        west =
            (n // 8)
                |> modBy 2
                |> toStatus
    in
    { north = north, east = east, south = south, west = west }
