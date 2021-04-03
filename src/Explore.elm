module Explore exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (Configuration, Error(..), Maze)
import Svg
import Svg.Attributes as SvgAttribute


main =
    Browser.element
        { init = init
        , update = update
        , view = view { size = 640, barrierColor = "black", gridColor = "seashell", cellColor = "white" }
        , subscriptions = subscriptions
        }


type alias Model =
    Result Error Maze


type alias Data =
    { maze : Maze
    }


type Error
    = MazeError Maze.Error


maze : String
maze =
    [ "############"
    , "#..........#"
    , "#...######.#"
    , "###.#....#.#"
    , "#......#.#.#"
    , "#.#....#.#.#"
    , "#..#####.#.#"
    , "#...#....#.#"
    , "#.###...##.#"
    , "#..........#"
    , "############"
    ]
        |> String.join "\n"


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            maze
                |> Maze.fromDescription
                |> Result.mapError MazeError
    in
    ( model, Cmd.none )


type Msg
    = MazeMessage Maze.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Configuration -> Model -> Html Msg
view configuration model =
    model
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
