module Explore exposing (main)

import Automaton exposing (Automaton, create)
import Automaton.Action exposing (action)
import Automaton.Compass exposing (Compass(..))
import Automaton.Program as Automat
import Automaton.Rule exposing (rule)
import Automaton.Surrounding exposing (CellType(..), Surrounding)
import Browser
import Css exposing (alignItems, center, displayFlex, flexDirection, flexStart, flexWrap, justifyContent, noWrap, row)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Maze exposing (Configuration, Maze, Msg(..))
import Maze.Description as Description exposing (Error)
import Time exposing (every)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view =
            view { size = 640, barrierColor = "black", gridColor = "seashell", cellColor = "white", machineColor = "seagreen" }
                >> Html.toUnstyled
        , subscriptions = subscriptions
        }


type alias Model =
    Result Error Data


type alias Data =
    { maze : Maze
    , automaton : Automaton
    , state : RunState
    }


type RunState
    = Pauzed
    | Running


type Error
    = MazeError Description.Error


mazeDescription : String
mazeDescription =
    [ "############"
    , "#..........#"
    , "#....#.....#"
    , "#....#.#...#"
    , "##.###.###.#"
    , "##.####..#.#"
    , "#...###....#"
    , "#..@####...#"
    , "#....#...#.#"
    , "#......#...#"
    , "#.....#....#"
    , "############"
    ]
        |> String.join "\n"


init : () -> ( Model, Cmd Msg )
init _ =
    let
        aMaze : Result Error Maze
        aMaze =
            mazeDescription
                |> Description.fromString
                |> Result.map Maze.maze
                |> Result.mapError MazeError

        model : Model
        model =
            aMaze
                |> Result.map
                    (\m ->
                        let
                            program : Automat.Program
                            program =
                                [ ( 0
                                  , -- North
                                    [ rule Free Occupied Free Free <| action 0 North
                                    , rule Free Free Free Free <| action 1 East
                                    , rule Occupied Occupied Free Free <| action 2 West
                                    , rule Free Occupied Free Occupied <| action 0 North
                                    , rule Occupied Occupied Free Occupied <| action 3 South
                                    , rule Occupied Free Free Free <| action 1 East
                                    , rule Occupied Free Free Occupied <| action 1 East
                                    , rule Free Free Free Occupied <| action 1 East
                                    ]
                                  )
                                , ( 1
                                  , -- East
                                    [ rule Free Free Occupied Free <| action 1 East
                                    , rule Free Free Free Free <| action 3 South
                                    , rule Free Occupied Occupied Free <| action 0 North
                                    , rule Occupied Free Occupied Free <| action 1 East
                                    , rule Occupied Occupied Occupied Free <| action 2 West
                                    , rule Free Occupied Free Free <| action 3 South
                                    , rule Occupied Occupied Free Free <| action 3 South
                                    , rule Occupied Free Free Free <| action 3 South
                                    ]
                                  )
                                , ( 2
                                  , -- West
                                    [ rule Occupied Free Free Free <| action 2 West
                                    , rule Free Free Free Free <| action 0 North
                                    , rule Occupied Free Free Occupied <| action 3 South
                                    , rule Occupied Free Occupied Free <| action 2 West
                                    , rule Occupied Free Occupied Occupied <| action 1 East
                                    , rule Free Free Free Occupied <| action 0 North
                                    , rule Free Free Occupied Occupied <| action 0 North
                                    , rule Free Free Occupied Free <| action 0 North
                                    ]
                                  )
                                , ( 3
                                  , -- South
                                    [ rule Free Free Free Occupied <| action 3 South
                                    , rule Free Free Free Free <| action 2 West
                                    , rule Free Free Occupied Occupied <| action 1 East
                                    , rule Free Occupied Free Occupied <| action 3 South
                                    , rule Free Occupied Occupied Occupied <| action 0 North
                                    , rule Free Free Occupied Free <| action 2 West
                                    , rule Free Occupied Occupied Free <| action 2 West
                                    , rule Free Occupied Free Free <| action 2 West
                                    ]
                                  )
                                ]
                                    |> Automat.fromList

                            automat : Automaton
                            automat =
                                create 0 program
                        in
                        { maze = m, automaton = automat, state = Pauzed }
                    )
    in
    ( model, Cmd.none )


type Msg
    = MazeMessage Maze.Msg
    | Step
    | Tick
    | Stop
    | Run


update : Msg -> Model -> ( Model, Cmd Msg )
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
                situation : Maybe Surrounding
                situation =
                    Maze.situation maze

                nextStep : Maybe ( Automaton, Compass )
                nextStep =
                    situation
                        |> Maybe.andThen (\s -> Automaton.step s automaton)
            in
            case nextStep of
                Just ( nextAutomaton, heading ) ->
                    let
                        ( nextMaze, cmd ) =
                            Maze.update (Move heading) maze
                    in
                    ( Ok { data | automaton = nextAutomaton, maze = nextMaze }, Cmd.map MazeMessage cmd )

                Nothing ->
                    ( Ok data, Cmd.none )

        ( Stop, Ok data ) ->
            ( Ok { data | state = Pauzed }, Cmd.none )

        ( Run, Ok data ) ->
            ( Ok { data | state = Running }, Cmd.none )

        ( Tick, Ok { state } ) ->
            case state of
                Running ->
                    update Step model

                Pauzed ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateMaze : Maze.Msg -> Data -> ( Data, Cmd Maze.Msg )
updateMaze msg ({ maze } as data) =
    let
        ( nextMaze, cmd ) =
            Maze.update msg maze
    in
    ( { data | maze = nextMaze }, cmd )


view : Configuration -> Model -> Html Msg
view configuration model =
    Html.div []
        [ viewControls
        , Html.div
            [ Attribute.css
                [ displayFlex
                , flexDirection row
                , flexWrap noWrap
                , justifyContent center
                , alignItems flexStart
                ]
            ]
            [ viewMaze configuration model
            , viewAutomaton model
            ]
        ]


viewControls : Html Msg
viewControls =
    Html.div
        [ Attribute.css
            [ displayFlex
            , flexDirection row
            , flexWrap noWrap
            , justifyContent center
            ]
        ]
        [ Html.button [ Event.onClick Stop ] [ Html.text "⏹" ]
        , Html.button [ Event.onClick Step ] [ Html.text "🢒" ]
        , Html.button [ Event.onClick Run ] [ Html.text "🢒🢒" ]
        ]


viewMaze : Configuration -> Model -> Html Msg
viewMaze configuration model =
    model
        |> Result.map .maze
        |> Result.map (Maze.view configuration)
        |> Result.map Html.fromUnstyled
        |> Result.map (Html.map MazeMessage)
        |> withDefault broken


withDefault : (e -> a) -> Result e a -> a
withDefault transform result =
    case result of
        Ok verbatim ->
            verbatim

        Err e ->
            transform e


viewAutomaton : Model -> Html Msg
viewAutomaton model =
    model
        |> Result.map .automaton
        |> Result.map Automaton.view
        |> withDefault empty


broken : Error -> Html Msg
broken error =
    let
        errorMessage : String
        errorMessage =
            case error of
                MazeError e ->
                    Description.errorToString e
    in
    Html.div []
        [ Html.p [] [ Html.text "Something went wrong" ]
        , Html.pre [] [ Html.text errorMessage ]
        ]


empty : Error -> Html Msg
empty _ =
    Html.div [] []


subscriptions : Model -> Sub Msg
subscriptions _ =
    every 250 (\_ -> Tick)
