module Explore exposing (..)

import Automaton exposing (Automaton, Compass(..), Status(..), action, automaton, rule)
import Browser
import Dict
import Html exposing (Html)
import Maze exposing (Configuration, Error(..), Maze)


main : Program () (Model {}) Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view { size = 640, barrierColor = "black", gridColor = "seashell", cellColor = "white" }
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


maze : String
maze =
    [ "############"
    , "#..........#"
    , "#..........#"
    , "#....###...#"
    , "#..######..#"
    , "#..####....#"
    , "#...###....#"
    , "#...####...#"
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
            maze
                |> Maze.fromDescription
                |> Result.mapError MazeError

        rules =
            Dict.empty
                |> Dict.insert 0
                    -- North
                    [ rule Free Occupied Free Free <| action 0 North
                    , rule Free Free Free Free <| action 1 East
                    , rule Occupied Occupied Free Free <| action 2 West
                    ]
                |> Dict.insert 1
                    -- East
                    [ rule Free Free Occupied Free <| action 1 East
                    , rule Free Free Free Free <| action 3 South
                    , rule Free Occupied Occupied Free <| action 0 North
                    ]
                |> Dict.insert 2
                    -- West
                    [ rule Occupied Free Free Free <| action 2 West
                    , rule Free Free Free Free <| action 0 North
                    , rule Occupied Free Free Occupied <| action 3 South
                    ]
                |> Dict.insert 3
                    -- South
                    [ rule Free Free Free Occupied <| action 3 South
                    , rule Free Free Free Free <| action 2 West
                    , rule Free Free Occupied Occupied <| action 1 East
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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Configuration -> Model a -> Html Msg
view configuration model =
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
                MazeError UnknownCharacter ->
                    "Unknown character"

                MazeError TooFewRows ->
                    "Too few rows"

                MazeError TooFewColumns ->
                    "Too few columns"

                MazeError ColumnsDoNotAgree ->
                    "Columns do not agree"
    in
    Html.div []
        [ Html.p [] [ Html.text "Something went wrong" ]
        , Html.pre [] [ Html.text errorMessage ]
        ]


subscriptions : Model a -> Sub Msg
subscriptions _ =
    Sub.none
