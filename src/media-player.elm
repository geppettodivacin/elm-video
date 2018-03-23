port module Main exposing (..)

import Color
import Debug
import DOM as Dom
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Style exposing (Style, StyleSheet)
import Style.Border as Border
import Style.Background as Background
import Style.Font as Font
import Style.Color as Color


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
    | SeekToClicked Float


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

        SeekToClicked position ->
            model
                => pushVideoEvent (SeekTo position)



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

TODO: Fix this comment.

-}
type Style
    = Style
    | BodyStyle
    | HeaderStyle
    | PlayerStyle
    | VideoStyle
    | ButtonStyle ButtonType


type ButtonType
    = PlayButton
    | PauseButton
    | StopButton
    | VolumePlusButton
    | VolumeMinusButton
    | MuteButton
    | UnmuteButton
    | ReplayButton


stylesheet : StyleSheet Style variation
stylesheet =
    let
        buttonBackgroundPosition buttonType =
            case buttonType of
                PlayButton ->
                    ( 0, 0 )

                PauseButton ->
                    ( -19, 0 )

                StopButton ->
                    ( -38, 0 )

                VolumePlusButton ->
                    ( -57, 0 )

                VolumeMinusButton ->
                    ( -76, 0 )

                MuteButton ->
                    ( -95, 0 )

                UnmuteButton ->
                    ( -114, 0 )

                ReplayButton ->
                    ( -130, 0 )

        buttonStyle buttonType =
            Style.style (ButtonStyle buttonType)
                [ Border.none
                , Style.cursor "pointer"
                , Background.imageWith
                    { src = "images/buttons.png"
                    , repeat = Background.noRepeat
                    , size = Background.natural
                    , position = buttonBackgroundPosition buttonType
                    }
                , Style.prop "text-indent" "-99999px"
                ]
    in
        Style.styleSheet
            [ Style.style Style []
            , Style.style BodyStyle
                [ Font.typeface [ Font.font "verdana" ]
                ]
            , Style.style HeaderStyle
                [ Font.size 28
                , Color.text <| Color.rgb 51 51 51
                ]
            , Style.style PlayerStyle
                [ Color.background <| Color.rgb 51 51 51
                ]
            , Style.style VideoStyle
                [ Border.all 1
                , Border.solid
                , Color.border <| Color.rgb 46 82 164
                , Color.background Color.black
                ]
            , buttonStyle PlayButton
            , buttonStyle PauseButton
            , buttonStyle StopButton
            , buttonStyle VolumePlusButton
            , buttonStyle VolumeMinusButton
            , buttonStyle MuteButton
            , buttonStyle UnmuteButton
            , buttonStyle ReplayButton
            ]


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        Element.column BodyStyle
            [ padding 8 ]
            [ Element.h1 HeaderStyle [ paddingXY 0 10 ] <|
                Element.text "Sample Media Player using Elm Style Elements"
            , mediaPlayerView model
            , currentEventView model.currentEvent
            ]


mediaPlayerView : Model -> Element Style variation Msg
mediaPlayerView model =
    Element.el PlayerStyle
        [ paddingTop 16
        , paddingBottom 8
        , paddingLeft 16
        , paddingRight 16
        , alignLeft
        ]
    <|
        Element.column PlayerStyle
            [ spacing 5 ]
            [ Element.node "video" <|
                Element.el VideoStyle
                    ([ width <| px 305
                     , height <| px 160
                     , id "media-video"
                     , attribute "src" "videos/big-buck-bunny_trailer.webm"
                     ]
                        ++ videoEvents
                    )
                    Element.empty
            , controlsView model
            ]


controlsView : Model -> Element Style variation Msg
controlsView model =
    let
        maxValue : Float -> Attribute variation Msg
        maxValue value =
            attribute "max" <| toString value

        curValue : Float -> Attribute variation Msg
        curValue value =
            attribute "value" <| toString value
    in
        Element.row Style
            [ spacing 6 ]
            [ Element.node "progress" <|
                Element.el Style
                    [ maxValue model.duration, curValue model.position ]
                    Element.empty
            , buttonView ReplayButton "Replay"
            , playPauseButtonView model.playState
            , buttonView StopButton "Stop"
            , buttonView VolumePlusButton "+"
            , buttonView VolumeMinusButton "-"
            , muteButtonView model.muteState
            ]


playPauseButtonView : PlayState -> Element Style variation Msg
playPauseButtonView state =
    case state of
        PlayingState ->
            buttonView PauseButton "Pause"

        PausedState ->
            buttonView PlayButton "Play"


muteButtonView : MuteState -> Element Style variation Msg
muteButtonView state =
    case state of
        MutedState ->
            buttonView UnmuteButton "Unmute"

        UnmutedState ->
            buttonView MuteButton "Mute"


buttonView : ButtonType -> String -> Element Style variation Msg
buttonView buttonType backupText =
    Element.button (ButtonStyle buttonType)
        [ width (px 16)
        , height (px 16)
        , onClick (buttonMsg buttonType)
        ]
    <|
        Element.text backupText


buttonMsg : ButtonType -> Msg
buttonMsg buttonType =
    case buttonType of
        PlayButton ->
            PlayClicked

        PauseButton ->
            PauseClicked

        StopButton ->
            StopClicked

        VolumePlusButton ->
            VolumeUpClicked

        VolumeMinusButton ->
            VolumeDownClicked

        MuteButton ->
            MuteClicked

        UnmuteButton ->
            UnmuteClicked

        ReplayButton ->
            RestartClicked


{-| This displays the last event that was handled and exists primarily so that
I could see real-time what was being handled and what wasn't.
-}
currentEventView : Maybe String -> Element style variation msg
currentEventView maybeEvent =
    Element.whenJust maybeEvent
        (\event -> Element.text <| "Current event: " ++ event)



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
videoEvents : List (Attribute variation Msg)
videoEvents =
    [ on "playing" (Decode.succeed NowPlaying)
    , on "pause" (Decode.succeed NowPaused)
    , on "durationchange" decodeDuration
    , on "timeupdate" decodePosition
    , on "volumechange" decodeVolume
    ]


seekEvent : Float -> Attribute variation Msg
seekEvent duration =
    on "click" (decodeSeek duration)


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
        Dom.target <|
            Decode.map2 NowAtVolume
                (Decode.field "volume" Decode.float)
                (Decode.map toMuteState <| Decode.field "muted" Decode.bool)


decodePosition : Decode.Decoder Msg
decodePosition =
    Dom.target <|
        Decode.map NowAtPosition
            (Decode.field "currentTime" Decode.float)


decodeDuration : Decode.Decoder Msg
decodeDuration =
    Dom.target <|
        Decode.map NowHasDuration
            (Decode.field "duration" Decode.float)


decodeSeek : Float -> Decode.Decoder Msg
decodeSeek duration =
    let
        calcSeek width offset =
            SeekToClicked <| (offset / width) * duration
    in
        Decode.map2 calcSeek
            (Dom.target Dom.offsetWidth)
            (Decode.field "offsetX" Decode.float)



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
    | SeekTo Float


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

        SeekTo position ->
            Encode.object
                [ "kind" => Encode.string "seekto"
                , "position" => Encode.float position
                ]



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
