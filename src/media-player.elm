module Main exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Player
import StyleSheet exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { players : List Player.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( players, rawCommands ) =
            [ Player.init "player1", Player.init "player2" ]
                |> List.unzip
    in
        { players = players }
            => batchPlayerCommands rawCommands


type Msg
    = VideoMsg Int Player.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VideoMsg index vidMsg ->
            let
                updatePlayer playerIndex playerState =
                    if index == playerIndex then
                        Player.update vidMsg playerState
                    else
                        playerState => Cmd.none

                ( newPlayers, rawCommands ) =
                    model.players
                        |> List.indexedMap updatePlayer
                        |> List.unzip
            in
                { model | players = newPlayers }
                    => batchPlayerCommands rawCommands


subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        viewPlayer index player =
            Element.map (VideoMsg index) <|
                Player.mediaPlayerView player
    in
        Element.layout stylesheet <|
            row DefaultStyle
                [ spacing 30 ]
                (List.indexedMap viewPlayer model.players)


batchPlayerCommands : List (Cmd Player.Msg) -> Cmd Msg
batchPlayerCommands commands =
    commands
        |> List.indexedMap (\index -> Cmd.map (VideoMsg index))
        |> Cmd.batch


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
