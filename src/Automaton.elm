module Automaton exposing (Action, Automaton, Rule, Situation, CellType(..), action, automaton, rule, step, view)

import Automaton.Compass as Compass exposing (Compass)
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


type alias Rule a =
    { a
        | north : CellType
        , east : CellType
        , south : CellType
        , west : CellType
        , action : Action
    }


rule : CellType -> CellType -> CellType -> CellType -> Action -> Rule {}
rule north east south west anAction =
    { north = north, east = east, south = south, west = west, action = anAction }


type CellType
    = Occupied
    | Free


type alias Action =
    { nextState : State
    , direction : Compass
    }


action : State -> Compass -> Action
action nextState direction =
    { nextState = nextState, direction = direction }


type alias Situation =
    { north : CellType
    , east : CellType
    , south : CellType
    , west : CellType
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

        situations : List (Html msg)
        situations =
            List.range 0 15
                |> List.map situationFromInt
                |> List.map viewSituationHeader
    in
    Html.table [ Attribute.css [ borderCollapse collapse ] ]
        [ Html.thead []
            [ Html.tr [] <| Html.td [] [] :: situations
            ]
        , Html.tbody []
            rows
        ]


viewSituationHeader : Situation -> Html msg
viewSituationHeader { north, east, south, west } =
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
        situations : List (Html msg)
        situations =
            List.range 0 15
                |> List.map situationFromInt
                |> List.map (\situation -> lookup situation rules)
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
            :: situations


viewAction : Rule a -> Html msg
viewAction aRule =
    let
        next : String
        next =
            aRule.action.nextState
                |> String.fromInt

        direction : String
        direction =
            Compass.toString aRule.action.direction
    in
    Html.td [] [ Html.text <| next ++ direction ]


situationFromInt : Int -> Situation
situationFromInt n =
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
