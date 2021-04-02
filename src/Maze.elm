module Maze exposing (Configuration, Error, Maze, Msg, fromDescription, update, view)

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
        , data : Dict Int (Dict Int Content)
        }


type Content
    = Barrier
    | Empty


fromDescription : String -> Result Error Maze
fromDescription input =
    let
        inputRows =
            String.split "\n" input

        toContent cell =
            case cell of
                "." ->
                    Empty

                _ ->
                    Barrier

        toData : Int -> String -> Dict Int Content
        toData x row =
            row
                |> String.split ""
                |> List.indexedMap (\y column -> ( y, toContent column ))
                |> Dict.fromList

        data : Dict Int (Dict Int Content)
        data =
            input
                |> String.split "\n"
                |> List.indexedMap (\x row -> ( x, toData x row ))
                |> Dict.fromList

        columns =
            inputRows
                |> List.map String.length
                |> List.foldl max 0

        rows =
            List.length inputRows
    in
    Ok <| Maze { rows = rows, columns = columns, data = data }


type Error
    = General


type Msg
    = DoNothing


update : Msg -> Maze -> ( Maze, Cmd Msg )
update _ aMaze =
    ( aMaze, Cmd.none )


view : Configuration -> Maze -> Svg msg
view configuration ((Maze { rows, columns, data }) as aMaze) =
    let
        dividers =
            max rows columns

        dividedConfiguration =
            divided dividers configuration
    in
    Svg.svg [ Attribute.width <| String.fromInt configuration.size, Attribute.height <| String.fromInt configuration.size ]
        [ background configuration
        , maze dividedConfiguration aMaze
        , grid dividedConfiguration
        ]


maze : Divided Configuration -> Maze -> Svg msg
maze configuration (Maze { rows, columns, data }) =
    let
        dr = (configuration.dividers - rows) // 2

        dc = (configuration.dividers - columns) // 2

        toData x d =
            d
                |> Dict.toList
                |> List.map (\( y, content ) -> ( x + dr, y + dc, content ))
    in
    data
        |> Dict.toList
        |> List.concatMap (\( x, d ) -> toData x d)
        |> List.map (toCell configuration)
        |> Svg.g []


toCell : Divided Configuration -> ( Int, Int, Content ) -> Svg msg
toCell configuration ( row, column, content ) =
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


background : Configuration -> Svg msg
background configuration =
    Svg.rect
        [ Attribute.width <| String.fromInt configuration.size
        , Attribute.height <| String.fromInt configuration.size
        , Attribute.fill configuration.barrierColor
        ]
        []


grid : Divided Configuration -> Svg msg
grid configuration =
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
