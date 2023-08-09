module Maze exposing (Configuration, Maze, Msg(..), maze, situation, update, view)

import Automaton.Compass exposing (Compass(..))
import Automaton.Location as Location exposing (Location)
import Automaton.Surrounding exposing (CellType(..), Surrounding, surrounding)
import Dict
import Maze.Cell as Cell exposing (Cell)
import Maze.Description exposing (Description)
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
    = Maze Description


maze : Description -> Maze
maze =
    Maze


situation : Maze -> Maybe Surrounding
situation (Maze { machine, cells }) =
    let
        toSituation : Location -> Surrounding
        toSituation location =
            surrounding
                (state <| lookup (Location.go North location))
                (state <| lookup (Location.go East location))
                (state <| lookup (Location.go South location))
                (state <| lookup (Location.go West location))

        lookup : Location -> Maybe Cell
        lookup location =
            Dict.get location cells

        state : Maybe Cell -> CellType
        state cell =
            cell
                |> Maybe.map Cell.toCellType
                |> Maybe.withDefault Occupied
    in
    machine
        |> Maybe.map toSituation


type Msg
    = Move Compass


update : Msg -> Maze -> ( Maze, Cmd Msg )
update message (Maze ({ machine } as m)) =
    case message of
        Move heading ->
            let
                nextMachine : Maybe Location
                nextMachine =
                    machine
                        |> Maybe.map (Location.go heading)
            in
            ( Maze { m | machine = nextMachine }, Cmd.none )


view : Configuration -> Maze -> Svg msg
view configuration ((Maze { rows, columns, machine }) as aMaze) =
    let
        dividers : Int
        dividers =
            max rows columns

        dividedConfiguration : Divided Configuration
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
viewMaze configuration (Maze { cells }) =
    cells
        |> Dict.toList
        |> List.map (viewCell configuration)
        |> Svg.g []


viewCell : Divided Configuration -> ( Location, Cell ) -> Svg msg
viewCell configuration ( ( column, row ), content ) =
    let
        gridSize : Float
        gridSize =
            toFloat configuration.size / toFloat configuration.dividers

        color : String
        color =
            if Cell.isEmpty content then
                configuration.cellColor

            else
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


viewMachine : Divided Configuration -> Maybe Location -> Svg msg
viewMachine configuration location =
    let
        gridSize : Float
        gridSize =
            toFloat configuration.size / toFloat configuration.dividers

        content : List (Svg msg)
        content =
            location
                |> Maybe.map toMachine
                |> Maybe.map (\c -> [ c ])
                |> Maybe.withDefault []

        toMachine : Location -> Svg msg
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
        gridSize : Float
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
