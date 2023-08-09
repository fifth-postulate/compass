module MazeTest exposing (suite)

import Expect
import Maze exposing (Maze, maze)
import Maze.Description as Description exposing (Error(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Maze"
        [ describe "fromDescription"
            [ describe "errors"
                [ problem "unknown character" "," UnknownCharacter
                , problem "one row is not enough" "#" TooFewRows
                , problem "two rows is not enough" "#\n#" TooFewRows
                , problem "columns not in agreement" "#\n#\n##" ColumnsDoNotAgree
                , problem "one column is not enough" "#\n#\n#" TooFewColumns
                , problem "two column is not enough" "##\n##\n##" TooFewColumns
                , problem "at most one automaton" "####\n#@@#\n####" TooManyAutomata
                ]
            ]
        ]


problem : String -> String -> Error -> Test
problem testName description error =
    test testName <|
        \_ ->
            let
                actual : Result Error Maze
                actual =
                    description
                        |> Description.fromString
                        |> Result.map maze
            in
            Expect.equal actual <| Err error
