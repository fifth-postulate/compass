module Maze.Description exposing (Description, Error(..), errorToString, fromString)

import Automaton.Location exposing (Location)
import Dict exposing (Dict)
import Maze.Cell as Cell exposing (Cell(..))
import Specification exposing (Specification, fromPredicate)


type alias Description =
    { rows : Int
    , columns : Int
    , cells : Dict Location Cell
    , machine : Maybe Location
    }


type alias Raw =
    List (List Cell)


fromString : String -> Result Error Description
fromString input =
    let
        checkedInput : Result Error (List (List Cell))
        checkedInput =
            Ok input
                |> containsOnly [ '#', '.', '@', '\n' ]
                |> Result.map String.lines
                |> enoughRows
                |> Result.map (List.map toColumns)
                |> columnsAgree
                |> enoughColumns
                |> oneAutomata

        toColumns : String -> List Cell
        toColumns aRow =
            aRow
                |> String.split ""
                |> List.map Cell.fromString

        rows : Raw -> Int
        rows raw =
            raw
                |> List.length

        columns : Raw -> Int
        columns raw =
            raw
                |> List.map List.length
                |> List.foldl max 0

        cells : Raw -> Dict Location Cell
        cells raw =
            raw
                |> List.indexedMap
                    (\y row ->
                        List.indexedMap
                            (\x cell ->
                                ( ( x, y )
                                , if Cell.isBarrier cell then
                                    Barrier

                                  else
                                    Empty
                                )
                            )
                            row
                    )
                |> List.concat
                |> Dict.fromList

        machine : Raw -> Maybe Location
        machine raw =
            raw
                |> List.indexedMap (\y row -> List.indexedMap (\x cell -> ( x, y, cell )) row)
                |> List.concat
                |> locateMachine

        locateMachine : List ( Int, Int, Cell ) -> Maybe Location
        locateMachine cs =
            case cs of
                [] ->
                    Nothing

                ( x, y, candidate ) :: tails ->
                    if Cell.isMachine candidate then
                        Just ( x, y )

                    else
                        locateMachine tails
    in
    checkedInput
        |> Result.map (\raw -> { rows = rows raw, columns = columns raw, machine = machine raw, cells = cells raw })


containsOnly : List Char -> Specification Error String
containsOnly allowedCharacters =
    let
        predicate : String -> Bool
        predicate input =
            input
                |> String.all (\character -> List.member character allowedCharacters)
    in
    fromPredicate UnknownCharacter predicate


enoughRows : Specification Error (List String)
enoughRows =
    fromPredicate TooFewRows (\rows -> 3 <= List.length rows)


columnsAgree : Specification Error Raw
columnsAgree =
    let
        inAgreement : Raw -> Bool
        inAgreement result =
            let
                lengths : List Int
                lengths =
                    result
                        |> List.map List.length

                minimum : Int
                minimum =
                    lengths
                        |> List.minimum
                        |> Maybe.withDefault 0

                maximum : Int
                maximum =
                    lengths
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            minimum == maximum
    in
    fromPredicate ColumnsDoNotAgree inAgreement


enoughColumns : Specification Error Raw
enoughColumns =
    let
        atLeast3 : Raw -> Bool
        atLeast3 result =
            3
                <= (result
                        |> List.map List.length
                        |> List.minimum
                        |> Maybe.withDefault 0
                   )
    in
    fromPredicate TooFewColumns atLeast3


oneAutomata : Specification Error Raw
oneAutomata =
    let
        atMost1 : Raw -> Bool
        atMost1 result =
            1
                >= (result
                        |> List.concat
                        |> List.filter Cell.isMachine
                        |> List.length
                   )
    in
    fromPredicate TooManyAutomata atMost1


type Error
    = UnknownCharacter
    | TooFewRows
    | TooFewColumns
    | ColumnsDoNotAgree
    | TooManyAutomata


errorToString : Error -> String
errorToString error =
    case error of
        UnknownCharacter ->
            "unknown character"

        TooFewRows ->
            "too few rows"

        TooFewColumns ->
            "too few columns"

        ColumnsDoNotAgree ->
            "colums do not agree"

        TooManyAutomata ->
            "too many automata"
