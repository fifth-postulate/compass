module Maze exposing (Configuration, Error(..), Maze, Msg(..), errorToString, fromDescription, go, situation, update, view)

import Automaton exposing (Compass(..), Situation, Status(..))
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as Attribute


type alias Configuration =
    { size : Int
    , barrierColor : String
    , gridColor : String
    , cellColor : String
    , machineColor : String
    }


type alias Divided a =
    { a | dividers : Int }


type Maze
    = Maze
        { rows : Int
        , columns : Int
        , data : Dict Int (Dict Int Cell)
        , machine : Maybe ( Int, Int )
        }


type Cell
    = Barrier
    | Machine
    | Empty


fromDescription : String -> Result Error Maze
fromDescription input =
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
                |> List.map toCell

        toCell : String -> Cell
        toCell cell =
            case cell of
                "." ->
                    Empty

                "@" ->
                    Machine

                _ ->
                    Barrier

        rows : List (List Cell) -> Int
        rows raw =
            raw
                |> List.length

        columns : List (List Cell) -> Int
        columns raw =
            raw
                |> List.map List.length
                |> List.foldl max 0

        data : List (List Cell) -> Dict Int (Dict Int Cell)
        data raw =
            raw
                |> List.indexedMap (\y row -> ( y, toData row ))
                |> Dict.fromList

        toData : List Cell -> Dict Int Cell
        toData row =
            row
                |> List.indexedMap (\x cell -> ( x, occupied cell ))
                |> Dict.fromList

        occupied cell =
            case cell of
                Barrier ->
                    Barrier

                _ ->
                    Empty

        machine : List (List Cell) -> Maybe ( Int, Int )
        machine raw =
            raw
                |> List.indexedMap (\y row -> List.indexedMap (\x cell -> ( x, y, cell )) row)
                |> List.concat
                |> locate Machine

        locate : Cell -> List ( Int, Int, Cell ) -> Maybe ( Int, Int )
        locate target cells =
            case cells of
                [] ->
                    Nothing

                ( x, y, candidate ) :: tails ->
                    if candidate == target then
                        Just ( x, y )

                    else
                        locate target tails
    in
    checkedInput
        |> Result.map (\raw -> Maze { rows = rows raw, columns = columns raw, data = data raw, machine = machine raw })


type alias Specification e a =
    Result e a -> Result e a


toSpecification : e -> (a -> Bool) -> Specification e a
toSpecification error predicate source =
    let
        fromPredicate a =
            if predicate a then
                Ok a

            else
                Err error
    in
    source
        |> Result.andThen fromPredicate


containsOnly : List Char -> Specification Error String
containsOnly allowedCharacters =
    let
        predicate input =
            input
                |> String.all (\character -> List.member character allowedCharacters)
    in
    toSpecification UnknownCharacter predicate


enoughRows : Specification Error (List String)
enoughRows =
    toSpecification TooFewRows (\rows -> 3 <= List.length rows)


columnsAgree : Specification Error (List (List Cell))
columnsAgree =
    let
        inAgreement : List (List Cell) -> Bool
        inAgreement result =
            let
                lengths =
                    result
                        |> List.map List.length

                minimum =
                    lengths
                        |> List.minimum
                        |> Maybe.withDefault 0

                maximum =
                    lengths
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            minimum == maximum
    in
    toSpecification ColumnsDoNotAgree inAgreement


enoughColumns : Specification Error (List (List Cell))
enoughColumns =
    let
        atLeast3 : List (List Cell) -> Bool
        atLeast3 result =
            result
                |> List.map List.length
                |> List.minimum
                |> Maybe.withDefault 0
                |> (<=) 3
    in
    toSpecification TooFewColumns atLeast3


oneAutomata : Specification Error (List (List Cell))
oneAutomata =
    let
        atMost1 : List (List Cell) -> Bool
        atMost1 result =
            result
                |> List.concat
                |> List.filter ((==) Machine)
                |> List.length
                |> (>=) 1
    in
    toSpecification TooManyAutomata atMost1


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


situation : Maze -> Maybe Situation
situation (Maze { machine, data }) =
    let
        toSituation location =
            { north = state <| lookup (go North location)
            , east = state <| lookup (go East location)
            , south = state <| lookup (go South location)
            , west = state <| lookup (go West location)
            }

        lookup ( x, y ) =
            data
                |> Dict.get y
                |> Maybe.andThen (Dict.get x)

        state cell =
            cell
                |> Maybe.map cellToState
                |> Maybe.withDefault Occupied

        cellToState cell =
            case cell of
                Empty ->
                    Free

                _ ->
                    Occupied
    in
    machine
        |> Maybe.map toSituation


go : Compass -> ( Int, Int ) -> ( Int, Int )
go direction ( x, y ) =
    case direction of
        North ->
            ( x, y - 1 )

        East ->
            ( x + 1, y )

        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )


type Msg
    = Move Compass


update : Msg -> Maze -> ( Maze, Cmd Msg )
update message (Maze ({ machine } as data)) =
    case message of
        Move direction ->
            let
                nextMachine =
                    machine
                        |> Maybe.map (go direction)
            in
            ( Maze { data | machine = nextMachine }, Cmd.none )


view : Configuration -> Maze -> Svg msg
view configuration ((Maze { rows, columns, machine }) as aMaze) =
    let
        dividers =
            max rows columns

        dividedConfiguration =
            divided dividers configuration
    in
    Svg.svg [ Attribute.width <| String.fromInt configuration.size, Attribute.height <| String.fromInt configuration.size ]
        [ viewBackground configuration
        , viewMaze dividedConfiguration aMaze
        , viewMachine dividedConfiguration machine
        , viewGrid dividedConfiguration
        ]


viewMaze : Divided Configuration -> Maze -> Svg msg
viewMaze configuration (Maze { rows, columns, data }) =
    let
        dr =
            (configuration.dividers - rows) // 2

        dc =
            (configuration.dividers - columns) // 2

        toData y d =
            d
                |> Dict.toList
                |> List.map (\( x, cell ) -> ( x + dr, y + dc, cell ))
    in
    data
        |> Dict.toList
        |> List.concatMap (\( y, d ) -> toData y d)
        |> List.map (viewCell configuration)
        |> Svg.g []


viewCell : Divided Configuration -> ( Int, Int, Cell ) -> Svg msg
viewCell configuration ( column, row, content ) =
    let
        gridSize =
            toFloat configuration.size / toFloat configuration.dividers

        color =
            case content of
                Empty ->
                    configuration.cellColor

                _ ->
                    configuration.barrierColor
    in
    Svg.rect
        [ Attribute.x <| String.fromFloat <| (*) gridSize <| toFloat <| column
        , Attribute.y <| String.fromFloat <| (*) gridSize <| toFloat <| row
        , Attribute.width <| String.fromFloat gridSize
        , Attribute.height <| String.fromFloat gridSize
        , Attribute.fill color
        ]
        []


divided : Int -> Configuration -> Divided Configuration
divided dividers configuration =
    { size = configuration.size
    , barrierColor = configuration.barrierColor
    , gridColor = configuration.gridColor
    , cellColor = configuration.cellColor
    , machineColor = configuration.machineColor
    , dividers = dividers
    }


viewMachine : Divided Configuration -> Maybe ( Int, Int ) -> Svg msg
viewMachine configuration location =
    let
        gridSize =
            toFloat configuration.size / toFloat configuration.dividers

        content =
            location
                |> Maybe.map toMachine
                |> Maybe.map (\c -> [ c ])
                |> Maybe.withDefault []

        toMachine ( x, y ) =
            Svg.circle
                [ Attribute.cx <| String.fromFloat <| (*) gridSize <| (+) 0.5 <| toFloat x
                , Attribute.cy <| String.fromFloat <| (*) gridSize <| (+) 0.5 <| toFloat y
                , Attribute.r <| String.fromFloat <| (*) gridSize <| 0.45
                , Attribute.fill <| configuration.machineColor
                ]
                []
    in
    Svg.g [] content


viewBackground : Configuration -> Svg msg
viewBackground configuration =
    Svg.rect
        [ Attribute.width <| String.fromInt configuration.size
        , Attribute.height <| String.fromInt configuration.size
        , Attribute.fill configuration.barrierColor
        ]
        []


viewGrid : Divided Configuration -> Svg msg
viewGrid configuration =
    Svg.g [ Attribute.stroke configuration.gridColor ]
        [ horizontalLines configuration
        , verticalLines configuration
        ]


horizontalLines : Divided Configuration -> Svg msg
horizontalLines configuration =
    lines configuration <| horizontalLine configuration


verticalLines : Divided Configuration -> Svg msg
verticalLines configuration =
    lines configuration <| verticalLine configuration


lines : Divided Configuration -> (Float -> Svg msg) -> Svg msg
lines ({ dividers } as configuration) toLine =
    let
        gridSize =
            toFloat configuration.size / toFloat dividers
    in
    List.range 0 (dividers + 1)
        |> List.map (\i -> gridSize * toFloat i)
        |> List.map toLine
        |> Svg.g []


horizontalLine : Divided Configuration -> Float -> Svg msg
horizontalLine configuration x =
    Svg.line
        [ Attribute.x1 <| String.fromFloat x
        , Attribute.y1 <| String.fromFloat 0
        , Attribute.x2 <| String.fromFloat x
        , Attribute.y2 <| String.fromFloat <| toFloat configuration.size
        ]
        []


verticalLine : Divided Configuration -> Float -> Svg msg
verticalLine configuration y =
    Svg.line
        [ Attribute.x1 <| String.fromFloat 0
        , Attribute.y1 <| String.fromFloat y
        , Attribute.x2 <| String.fromFloat <| toFloat configuration.size
        , Attribute.y2 <| String.fromFloat y
        ]
        []
