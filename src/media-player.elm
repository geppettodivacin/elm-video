port module Main exposing (..)

import Debug
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
    , playState : PlayState
    , volumeState : VolumeState
    , position : Float
    , duration : Float
    }


type PlayState
    = PlayingState
    | PausedState


type MuteState
    = MutedState
    | UnmutedState


type alias VolumeState =
    ( Float, MuteState )


init : ( Model, Cmd Msg )
init =
    ( Model Nothing PausedState ( 1, UnmutedState ) 0 0, pushVideoEvent Setup )



-- UPDATE


type Msg
    = Event String
    | PlayClicked
    | PauseClicked
    | MuteClicked
    | UnmuteClicked
    | VolumeDownClicked
    | VolumeUpClicked
    | NowPlaying
    | NowPaused
    | NowAtVolume VolumeState
    | NowAtPosition Float
    | NowHasDuration Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Event event ->
            ( { model | currentEvent = Just event }, Cmd.none )

        NowPlaying ->
            ( { model | currentEvent = Just "playing", playState = PlayingState }, Cmd.none )

        NowPaused ->
            ( { model | currentEvent = Just "paused", playState = PausedState }, Cmd.none )

        NowAtVolume state ->
            ( { model | currentEvent = Just "volumechange", volumeState = state }, Cmd.none )

        NowAtPosition position ->
            ( { model | currentEvent = Just "timeupdate", position = position }, Cmd.none )

        NowHasDuration duration ->
            ( { model | currentEvent = Just "durationchange", duration = duration }, Cmd.none )

        PlayClicked ->
            ( model, pushVideoEvent Play )

        PauseClicked ->
            ( model, pushVideoEvent Pause )

        MuteClicked ->
            ( model, pushVideoEvent Mute )

        UnmuteClicked ->
            ( model, pushVideoEvent Unmute )

        VolumeDownClicked ->
            ( model, pushVideoEvent VolumeDown )

        VolumeUpClicked ->
            ( model, pushVideoEvent VolumeUp )



-- PORTS


type VideoEvent
    = Setup
    | Play
    | Pause
    | Mute
    | Unmute
    | VolumeDown
    | VolumeUp


port videoEventStream : Value -> Cmd msg


pushVideoEvent : VideoEvent -> Cmd msg
pushVideoEvent event =
    event
        |> encodeVideoEvent
        |> videoEventStream


encodeVideoEvent : VideoEvent -> Value
encodeVideoEvent event =
    case event of
        Setup ->
            Encode.object
                [ "kind" => Encode.string "setup" ]

        Play ->
            Encode.object
                [ "kind" => Encode.string "play" ]

        Pause ->
            Encode.object
                [ "kind" => Encode.string "pause" ]

        Mute ->
            Encode.object
                [ "kind" => Encode.string "mute" ]

        Unmute ->
            Encode.object
                [ "kind" => Encode.string "unmute" ]

        VolumeDown ->
            Encode.object
                [ "kind" => Encode.string "volumedown" ]

        VolumeUp ->
            Encode.object
                [ "kind" => Encode.string "volumeup" ]



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
            [ video ([ id "media-video" ] ++ videoEvents)
                [ source [ src "videos/big-buck-bunny_trailer.webm", type_ "video/mp4" ] []
                ]
            , div [ id "media-controls" ]
                [ progress [ id "progress-bar", Attr.min "0", Attr.max (toString model.duration), value (toString model.position) ] [ text "played" ]
                , button [ id "replay-button", class "replay", title "replay" ] [ text "Replay" ]
                , playPauseButton model.playState
                , button [ id "stop-button", class "stop", title "stop" ] [ text "Stop" ]
                , button [ id "volume-inc-button", class "volume-plus", title "increase volume", onClick VolumeUpClicked ] [ text "Increase Volume" ]
                , button [ id "volume-dec-button", class "volume-minus", title "decrease volume", onClick VolumeDownClicked ] [ text "Decrease Volume" ]
                , muteButton model.volumeState
                ]
            ]
        , currentEventView model.currentEvent
        ]


currentEventView : Maybe String -> Html msg
currentEventView maybeEvent =
    case maybeEvent of
        Nothing ->
            div [] []

        Just event ->
            div [] [ text <| "Current event: " ++ event ]


playPauseButton : PlayState -> Html Msg
playPauseButton state =
    case state of
        PlayingState ->
            button [ id "play-pause-button", class "pause", title "pause", onClick PauseClicked ] [ text "Pause" ]

        PausedState ->
            button [ id "play-pause-button", class "play", title "play", onClick PlayClicked ] [ text "Play" ]


muteButton : VolumeState -> Html Msg
muteButton ( _, state ) =
    case state of
        MutedState ->
            button [ id "mute-unmute-button", class "unmute", title "unmute", onClick UnmuteClicked ] [ text "Unmute" ]

        UnmutedState ->
            button [ id "mute-unmute-button", class "mute", title "mute", onClick MuteClicked ] [ text "Mute" ]



-- EVENTS


videoEvents : List (Attribute Msg)
videoEvents =
    [ on "playing" (Decode.succeed NowPlaying)
    , on "pause" (Decode.succeed NowPaused)
    , on "durationchange" decodeDuration
    , on "timeupdate" decodePosition
    , on "volumechange" decodeVolume
    ]


simpleEvent : String -> Attribute Msg
simpleEvent event =
    on event (Decode.succeed <| Event event)


decodeVolume : Decode.Decoder Msg
decodeVolume =
    let
        toMuteState muted =
            if muted then
                MutedState
            else
                UnmutedState
    in
        Decode.field "target" <|
            Decode.map2 (\volume muted -> NowAtVolume ( volume, toMuteState muted ))
                (Decode.field "volume" Decode.float)
                (Decode.field "muted" Decode.bool)


decodePosition : Decode.Decoder Msg
decodePosition =
    Decode.map NowAtPosition
        (Decode.at [ "target", "currentTime" ] Decode.float)


decodeDuration : Decode.Decoder Msg
decodeDuration =
    Decode.map NowHasDuration
        (Decode.at [ "target", "duration" ] Decode.float)



-- UTILITIES


infixl 0 =>
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
