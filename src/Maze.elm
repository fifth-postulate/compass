module Maze exposing (Configuration, Error, Maze, Msg, fromDescription, update, view)

import Svg exposing (Svg)
import Svg.Attributes as Attribute


type alias Configuration =
    { size : Int
    , barrierColor : String
    , gridColor : String
    }


type alias Divided a =
    { a | dividers : Int }


type Maze
    = Maze
        { rows : Int
        , columns : Int
        }


fromDescription : String -> Result Error Maze
fromDescription _ =
    Ok <| Maze { rows = 5, columns = 8 }


type Error
    = General


type Msg
    = DoNothing


update : Msg -> Maze -> ( Maze, Cmd Msg )
update _ maze =
    ( maze, Cmd.none )


view : Configuration -> Maze -> Svg Msg
view configuration (Maze { rows, columns }) =
    let
        dividers =
            2 + max rows columns
    in
    Svg.svg [ Attribute.width <| String.fromInt configuration.size, Attribute.height <| String.fromInt configuration.size ]
        [ background configuration
        , grid <| divided dividers configuration
        ]


divided : Int -> Configuration -> Divided Configuration
divided dividers configuration =
    { size = configuration.size
    , barrierColor = configuration.barrierColor
    , gridColor = configuration.gridColor
    , dividers = dividers
    }


background : Configuration -> Svg msg
background configuration =
    Svg.rect
        [ Attribute.width <| String.fromInt configuration.size
        , Attribute.height <| String.fromInt configuration.size
        , Attribute.color configuration.barrierColor
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
