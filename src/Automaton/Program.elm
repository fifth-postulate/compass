module Automaton.Program exposing (Program, fromList, rules, view)

import Automaton.Action as Action exposing (Action)
import Automaton.Compass as Compass
import Automaton.Rule as Rule exposing (Rule)
import Automaton.State exposing (State)
import Automaton.Surrounding as Surrounding exposing (CellType(..), Surrounding)
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


type Program
    = Program (Dict State (List Rule))


fromList : List ( State, List Rule ) -> Program
fromList =
    Dict.fromList >> Program


rules : State -> Program -> Maybe (List Rule)
rules state (Program program) =
    Dict.get state program


view : State -> Program -> Html msg
view current (Program program) =
    let
        states : List State
        states =
            program
                |> Dict.keys
                |> List.sort

        rows : List (Html msg)
        rows =
            states
                |> List.map (\state -> ( state, Dict.get state program |> Maybe.withDefault [] ))
                |> List.map (uncurry (viewState current))

        surrounding : List (Html msg)
        surrounding =
            List.range 0 15
                |> List.map Surrounding.fromInt
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
viewSurroundingHeader surrounding =
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
                , borderTopWidth <| toWidth <| Surrounding.north surrounding
                , borderRightWidth <| toWidth <| Surrounding.east surrounding
                , borderBottomWidth <| toWidth <| Surrounding.south surrounding
                , borderLeftWidth <| toWidth <| Surrounding.west surrounding
                ]
            ]
            []
        ]


viewState : State -> State -> List Rule -> Html msg
viewState current state someRules =
    let
        surroundings : List (Html msg)
        surroundings =
            List.range 0 15
                |> List.map Surrounding.fromInt
                |> List.map (flip Rule.lookup someRules)
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


viewAction : Rule -> Html msg
viewAction aRule =
    let
        action : Action
        action =
            Rule.action aRule

        next : String
        next =
            action
                |> Action.nextState
                |> String.fromInt

        heading : String
        heading =
            Compass.toString <| Action.heading action
    in
    Html.td [] [ Html.text <| next ++ heading ]


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
