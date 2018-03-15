port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr exposing (..)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentEvent : Maybe String
    , state : PlayState
    }


type PlayState
    = Playing
    | Paused


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Paused, Cmd.none )



-- UPDATE


type Msg
    = Event String
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Event event ->
            ( { model | currentEvent = Just event }, Cmd.none )

        Play ->
            ( { model | currentEvent = Just "playing", state = Playing }, Cmd.none )

        Pause ->
            ( { model | currentEvent = Just "paused", state = Paused }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    body []
        [ h1 [] [ text "Sample Media Player using HTML5's Media API" ]
        , div [ id "media-player" ]
            [ video ([ id "media-video", autoplay True ] ++ videoEvents)
                [ source [ src "videos/big-buck-bunny_trailer.webm", type_ "video/mp4" ] []
                ]
            , div [ id "media-controls" ]
                [ progress [ id "progress-bar", Attr.min "0", Attr.max "100", value "0" ] [ text "played" ]
                , button [ id "replay-button", class "replay", title "replay" ] [ text "Replay" ]
                , playPauseButton model.state
                , button [ id "stop-button", class "stop", title "stop" ] [ text "Stop" ]
                , button [ id "volume-inc-button", class "volume-plus", title "increase volume" ] [ text "Increase Volume" ]
                , button [ id "volume-dec-button", class "volume-minus", title "decrease volume" ] [ text "Decrease Volume" ]
                , button [ id "mute-button", class "mute", title "mute" ] [ text "Mute" ]
                ]
            ]
        , currentEventView model.currentEvent
        ]


simpleEvent : String -> Attribute Msg
simpleEvent event =
    on event (Decode.succeed <| Event event)


videoEvents : List (Attribute Msg)
videoEvents =
    [ on "playing" (Decode.succeed Play)
    , simpleEvent "timeupdate"
    , simpleEvent "ended"
    , on "pause" (Decode.succeed Pause)
    ]


currentEventView : Maybe String -> Html msg
currentEventView maybeEvent =
    case maybeEvent of
        Nothing ->
            div [] []

        Just event ->
            div [] [ text <| "Current event: " ++ event ]


playPauseButton : PlayState -> Html msg
playPauseButton state =
    case state of
        Playing ->
            button [ id "play-pause-button", class "pause", title "pause" ] [ text "Pause" ]

        Paused ->
            button [ id "play-pause-button", class "play", title "play" ] [ text "Play" ]
