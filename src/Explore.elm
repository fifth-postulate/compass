module Explore exposing (..)

import Browser
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> (Model, Cmd Msg)
init _ =
    ({}, Cmd.none)


type alias Model =
    {}


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    Html.text "Hello, World"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
