module Explore exposing (..)

import Browser
import Html exposing (Html)
import Maze exposing (Configuration, Maze)
import Svg
import Svg.Attributes as SvgAttribute


main =
    Browser.element
        { init = init
        , update = update
        , view = view { size = 640, barrierColor = "black", gridColor = "seashell" }
        , subscriptions = subscriptions
        }


type alias Model =
    Result Error Maze


type alias Data =
    { maze : Maze
    }


type Error
    = MazeError Maze.Error


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Maze.fromDescription ""
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
        |> Result.withDefault broken


broken : Html Msg
broken =
    Html.text "Something went wrong"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
