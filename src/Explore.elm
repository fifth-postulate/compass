module Explore exposing (..)

import Automaton exposing (Automaton, Compass(..), Status(..), action, automaton, rule)
import Browser
import Dict
import Html exposing (Html)
import Html.Events as Event
import Maze exposing (Configuration, Error, Maze, Msg(..))


main : Program () (Model {}) Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view { size = 640, barrierColor = "black", gridColor = "seashell", cellColor = "white", machineColor = "seagreen" }
        , subscriptions = subscriptions
        }


type alias Model a =
    Result Error (Data a)


type alias Data a =
    { maze : Maze
    , automaton : Automaton a
    }


type Error
    = MazeError Maze.Error


mazeDescription : String
mazeDescription =
    [ "############"
    , "#..........#"
    , "#..........#"
    , "#....#.#...#"
    , "##.###.##..#"
    , "##.####....#"
    , "#...###....#"
    , "#..@####...#"
    , "#....#.....#"
    , "#..........#"
    , "#..........#"
    , "############"
    ]
        |> String.join "\n"


init : () -> ( Model {}, Cmd Msg )
init _ =
    let
        aMaze =
            mazeDescription
                |> Maze.fromDescription
                |> Result.mapError MazeError

        rules =
            Dict.empty
                |> Dict.insert 0
                    -- North
                    [ rule Free Occupied Free Free <| action 0 North
                    , rule Free Free Free Free <| action 1 East
                    , rule Occupied Occupied Free Free <| action 2 West
                    , rule Free Occupied Free Occupied <| action 0 North
                    , rule Occupied Occupied Free Occupied <| action 3 South
                    ]
                |> Dict.insert 1
                    -- East
                    [ rule Free Free Occupied Free <| action 1 East
                    , rule Free Free Free Free <| action 3 South
                    , rule Free Occupied Occupied Free <| action 0 North
                    , rule Occupied Free Occupied Free <| action 1 East
                    , rule Occupied Occupied Occupied Free <| action 2 West
                    ]
                |> Dict.insert 2
                    -- West
                    [ rule Occupied Free Free Free <| action 2 West
                    , rule Free Free Free Free <| action 0 North
                    , rule Occupied Free Free Occupied <| action 3 South
                    , rule Occupied Free Occupied Free <| action 2 West
                    , rule Occupied Free Occupied Occupied <| action 1 East
                    ]
                |> Dict.insert 3
                    -- South
                    [ rule Free Free Free Occupied <| action 3 South
                    , rule Free Free Free Free <| action 2 West
                    , rule Free Free Occupied Occupied <| action 1 East
                    , rule Free Occupied Free Occupied <| action 3 South
                    , rule Free Occupied Occupied Occupied <| action 0 North
                    ]

        automat =
            automaton 0 rules

        model =
            aMaze
                |> Result.map (\m -> { maze = m, automaton = automat })
    in
    ( model, Cmd.none )


type Msg
    = MazeMessage Maze.Msg
    | Step


update : Msg -> Model a -> ( Model a, Cmd Msg )
update message model =
    case ( message, model ) of
        ( MazeMessage msg, Ok data ) ->
            let
                ( nextData, c ) =
                    updateMaze msg data
            in
            ( Ok nextData, Cmd.map MazeMessage c )

        ( Step, Ok ({ maze, automaton } as data) ) ->
            let
                situation =
                    Maze.situation maze

                nextStep =
                    situation
                        |> Maybe.andThen (\s -> Automaton.step s automaton)
            in
            case nextStep of
                Just ( nextAutomaton, direction ) ->
                    let
                        ( nextMaze, cmd ) =
                            Maze.update (Move direction) maze
                    in
                    ( Ok { data | automaton = nextAutomaton, maze = nextMaze }, Cmd.map MazeMessage cmd )

                Nothing ->
                    ( Ok data, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateMaze : Maze.Msg -> Data a -> ( Data a, Cmd Maze.Msg )
updateMaze msg ({ maze } as data) =
    let
        ( nextMaze, cmd ) =
            Maze.update msg maze
    in
    ( { data | maze = nextMaze }, cmd )


view : Configuration -> Model a -> Html Msg
view configuration model =
    Html.div []
        [ viewControls
        , Html.div []
            [ viewMaze configuration model
            ]
        ]


viewControls : Html Msg
viewControls =
    Html.div []
        [ Html.button [ Event.onClick Step ] [ Html.text "🢒" ]
        ]


viewMaze : Configuration -> Model a -> Html Msg
viewMaze configuration model =
    model
        |> Result.map .maze
        |> Result.map (Maze.view configuration)
        |> Result.map (Html.map MazeMessage)
        |> withDefault broken


withDefault : (e -> a) -> Result e a -> a
withDefault transform result =
    case result of
        Ok verbatim ->
            verbatim

        Err e ->
            transform e


broken : Error -> Html Msg
broken error =
    let
        errorMessage =
            case error of
                MazeError e ->
                    Maze.errorToString e
    in
    Html.div []
        [ Html.p [] [ Html.text "Something went wrong" ]
        , Html.pre [] [ Html.text errorMessage ]
        ]


subscriptions : Model a -> Sub Msg
subscriptions _ =
    Sub.none
