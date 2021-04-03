module Maze exposing (Configuration, Error(..), Maze, Msg, fromDescription, update, view)

import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as Attribute


type alias Configuration =
    { size : Int
    , barrierColor : String
    , gridColor : String
    , cellColor : String
    }


type alias Divided a =
    { a | dividers : Int }


type Maze
    = Maze
        { rows : Int
        , columns : Int
        , data : Dict Int (Dict Int Cell)
        }


type Cell
    = Barrier
    | Empty


fromDescription : String -> Result Error Maze
fromDescription input =
    let
        checkedInput : Result Error (List (List Cell))
        checkedInput =
            Ok input
                |> containsOnly [ '#', '.', '\n' ]
                |> Result.map String.lines
                |> enoughRows
                |> Result.map (List.map toColumns)
                |> columnsAgree
                |> enoughColumns

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
                |> List.indexedMap (\x cell -> ( x, cell ))
                |> Dict.fromList
    in
    checkedInput
        |> Result.map (\raw -> Maze { rows = rows raw, columns = columns raw, data = data raw })


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


type Error
    = UnknownCharacter
    | TooFewRows
    | TooFewColumns
    | ColumnsDoNotAgree


type Msg
    = DoNothing


update : Msg -> Maze -> ( Maze, Cmd Msg )
update _ aMaze =
    ( aMaze, Cmd.none )


view : Configuration -> Maze -> Svg msg
view configuration ((Maze { rows, columns }) as aMaze) =
    let
        dividers =
            max rows columns

        dividedConfiguration =
            divided dividers configuration
    in
    Svg.svg [ Attribute.width <| String.fromInt configuration.size, Attribute.height <| String.fromInt configuration.size ]
        [ viewBackground configuration
        , viewMaze dividedConfiguration aMaze
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
                Barrier ->
                    configuration.barrierColor

                Empty ->
                    configuration.cellColor
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
    , dividers = dividers
    }


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
