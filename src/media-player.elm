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
    { currentEvent : Maybe String -- This is for debugging which events are handled.
    , playState : PlayState
    , muteState : MuteState
    , volume : Float -- Percentage from 0 - 1
    , position : Float -- In seconds
    , duration : Float -- In seconds
    }


type PlayState
    = PlayingState
    | PausedState


type MuteState
    = MutedState
    | UnmutedState


{-| This is the initial state and also the initialization command sent to
JavaScript to setup the video player.
-}
init : ( Model, Cmd Msg )
init =
    { currentEvent = Nothing
    , playState = PausedState
    , muteState = UnmutedState
    , volume = 1
    , position = 0
    , duration = 0
    }
        => pushVideoEvent Setup



-- UPDATE


{-| These are the kinds of internal messages that can be sent to the update
function. You'll note that I have used a few naming conventions:

    1. Messages ending in "Clicked" are the user clicking on parts of the
    interface.
    2. Messages starting with "Now" are messages received from the video player
    indicating a change in player state.

Each of these cases must be handled by the update function. Elm will throw a
compile-time error if you forget, though, so don't worry about it.

-}
type Msg
    = PlayClicked
    | PauseClicked
    | StopClicked
    | RestartClicked
    | MuteClicked
    | UnmuteClicked
    | VolumeDownClicked
    | VolumeUpClicked
    | NowPlaying
    | NowPaused
    | NowAtVolume Float MuteState
    | NowAtPosition Float
    | NowHasDuration Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NowPlaying ->
            { model | currentEvent = Just "playing", playState = PlayingState }
                => Cmd.none

        NowPaused ->
            { model | currentEvent = Just "paused", playState = PausedState }
                => Cmd.none

        NowAtVolume volume muteState ->
            { model | currentEvent = Just "volumechange", volume = volume, muteState = muteState }
                => Cmd.none

        NowAtPosition position ->
            { model | currentEvent = Just "timeupdate", position = position }
                => Cmd.none

        NowHasDuration duration ->
            { model | currentEvent = Just "durationchange", duration = duration }
                => Cmd.none

        PlayClicked ->
            model
                => pushVideoEvent Play

        PauseClicked ->
            model
                => pushVideoEvent Pause

        StopClicked ->
            model
                => pushVideoEvent Stop

        RestartClicked ->
            model
                => pushVideoEvent Restart

        MuteClicked ->
            model
                => pushVideoEvent Mute

        UnmuteClicked ->
            model
                => pushVideoEvent Unmute

        VolumeDownClicked ->
            model
                => pushVideoEvent VolumeDown

        VolumeUpClicked ->
            model
                => pushVideoEvent VolumeUp



-- PORTS


{-| This is how we send the video player messages. We are sending out a JSON
object with the command, where `kind` is always a string with the operation
name. Different commands may require additional information to be executed.
These objects should never be constructed by hand but should instead be sent
via the `pushVideoEvent` function.
-}
port videoEventStream : Value -> Cmd msg


{-| These are all the kinds of messages that can be sent to the video player.
Add more cases if we want to tell the video player new things.
-}
type VideoEvent
    = Setup
    | Play
    | Pause
    | Stop
    | Restart
    | Mute
    | Unmute
    | VolumeDown
    | VolumeUp


{-| This is the function we should use to send messages to the video player. It
takes care of encoding and pushing through the port.
-}
pushVideoEvent : VideoEvent -> Cmd msg
pushVideoEvent event =
    event
        |> encodeVideoEvent
        |> videoEventStream


{-| Encodes a VideoEvent as a simple JSON value. As new events are added, also
add a case for the encoder. Elm will throw a compile-time error if you forget,
so don't worry about forgetting.
-}
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

        Stop ->
            Encode.object
                [ "kind" => Encode.string "stop" ]

        Restart ->
            Encode.object
                [ "kind" => Encode.string "restart" ]

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


{-| The view should look very much like the original HTML, with some notable
differences. Changeable buttons (play/pause and mute/unmute) are added using
custom functions, since switching these values out inline based on the model
would take up a lot of room and would look ugly. You'll also note that the
value for the progress bar is set directly by the model value instead of to 0,
and it automatically updates as we change the model.
-}
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
                , button [ id "replay-button", class "replay", title "replay", onClick RestartClicked ] [ text "Replay" ]
                , playPauseButton model.playState
                , button [ id "stop-button", class "stop", title "stop", onClick StopClicked ] [ text "Stop" ]
                , button [ id "volume-inc-button", class "volume-plus", title "increase volume", onClick VolumeUpClicked ] [ text "Increase Volume" ]
                , button [ id "volume-dec-button", class "volume-minus", title "decrease volume", onClick VolumeDownClicked ] [ text "Decrease Volume" ]
                , muteButton model.muteState
                ]
            ]
        , currentEventView model.currentEvent -- Added for debugging events.
        ]


{-| This displays the last event that was handled and exists primarily so that
I could see real-time what was being handled and what wasn't.
-}
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


muteButton : MuteState -> Html Msg
muteButton state =
    case state of
        MutedState ->
            button [ id "mute-unmute-button", class "unmute", title "unmute", onClick UnmuteClicked ] [ text "Unmute" ]

        UnmutedState ->
            button [ id "mute-unmute-button", class "mute", title "mute", onClick MuteClicked ] [ text "Mute" ]



-- EVENTS


{-| A list of events that the video player may exhibit. We catch these and send
messages to our update function based on decoding the event object. We use
decoders here in case the JSON object does not conform to our expectations.
(Obviously it should, but with JavaScript, you're never sure.)

Note that for "playing" and "pause" events, we don't need to inspect the JSON
and just produce a message. All we care about here is that the event was fired.

For the others, we extract some information from the target of the video player
event, which is always the video media player object. This is used to update
our model. Note that this is because we can never have a direct, mutable
reference to the video, since then the runtime wouldn't know when the model is
actually updated.

-}
videoEvents : List (Attribute Msg)
videoEvents =
    [ on "playing" (Decode.succeed NowPlaying)
    , on "pause" (Decode.succeed NowPaused)
    , on "durationchange" decodeDuration
    , on "timeupdate" decodePosition
    , on "volumechange" decodeVolume
    ]


decodeVolume : Decode.Decoder Msg
decodeVolume =
    let
        -- This converts the boolean `muted` field of the media player to our
        -- more meaningful custom data type.
        toMuteState muted =
            if muted then
                MutedState
            else
                UnmutedState
    in
        Decode.field "target" <|
            Decode.map2 NowAtVolume
                (Decode.field "volume" Decode.float)
                (Decode.map toMuteState <| Decode.field "muted" Decode.bool)


decodePosition : Decode.Decoder Msg
decodePosition =
    Decode.map NowAtPosition
        (Decode.at [ "target", "currentTime" ] Decode.float)


decodeDuration : Decode.Decoder Msg
decodeDuration =
    Decode.map NowHasDuration
        (Decode.at [ "target", "duration" ] Decode.float)



-- UTILITIES


{-| This is just a useful alias for making tuples in places where commas have a
different meaning. It's super useful in lists for this reason.

As an example:

    [ ( "Steelers", 27 ), ( "Ravens", 0 ) ]

could be written:

    [ "Steelers" => 27, "Ravens" => 0 ]

This is often easier to read because you don't have to keep track of so many
commas and parentheses.

-}
infixl 0 =>
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
