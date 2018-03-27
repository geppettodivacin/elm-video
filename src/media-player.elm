port module Main exposing (..)

import Debug
import DOM as Dom
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import StyleSheet exposing (..)


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
    , useFake : Bool
    , w : Int
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
    , useFake = False
    , w = 0
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
    | FakeClicked


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
            { model | currentEvent = Just "timeupdate", position = position, w = model.w + 1 }
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

        FakeClicked ->
            { model | useFake = not model.useFake }
                => pushVideoEvent (Replace)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


{-| This performs the same function that HTML normally would. It defines what
the user will see on the page. The style elements package allows us to specify
the layout here in the view rather than in the CSS (or even in the stylesheet
in our custom StyleSheet module, which I factored out just to save space). As a
result, there is a lot more information in the view than would normally go in
HTML, such as padding, spacing, and sizes. Advantages here is that we can
change sizes and positions directly based on the model's values, and we only
have a single place to change any of our layout. The primary disadvantage is a
little more visual clutter here, and possibly some slowdown in rendering (since
the generated HTML is a lot busier than using the standard `Html` library).
-}
view : Model -> Html Msg
view model =
    Element.layout stylesheet <| viewMain model


viewMain : Model -> Element Class variation Msg
viewMain model =
    column BodyStyle
        [ padding 8 ]
        [ h1 HeaderStyle [ paddingXY 0 10 ] <|
            text "Sample Media Player using Elm Style Elements"
        , mediaPlayerView model
        , currentEventView model.currentEvent
        ]


mediaPlayerView : Model -> Element Class variation Msg
mediaPlayerView model =
    el PlayerStyle
        [ paddingTop 16
        , paddingBottom 8
        , paddingLeft 16
        , paddingRight 16
        , alignLeft
        ]
    <|
        column PlayerStyle
            [ spacing 5 ]
            [ if model.useFake then
                el DefaultStyle [] (videoView (model.w - 50))
              else
                videoView model.w
            , controlsView model
            ]


videoView : Int -> Element Class variation Msg
videoView x =
    let
        attributes =
            List.concat
                [ [ width <| px (305 - toFloat x)
                  , height <| px (160 - toFloat x)
                  , id "media-video"
                  , attribute "src" "videos/big-buck-bunny_trailer.webm"
                  ]
                , videoEvents
                ]
    in
        node "video" <|
            el VideoStyle attributes empty


controlsView : Model -> Element Class variation Msg
controlsView model =
    let
        maxValue : Float -> Attribute variation Msg
        maxValue value =
            attribute "max" <| toString value

        curValue : Float -> Attribute variation Msg
        curValue value =
            attribute "value" <| toString value
    in
        row DefaultStyle
            [ spacing 6 ]
            [ node "progress" <|
                el DefaultStyle
                    [ maxValue model.duration
                    , curValue model.position
                    , seekEvent model.duration
                    ]
                    empty
            , buttonView ReplayButton "Replay"
            , playPauseButtonView model.playState
            , buttonView StopButton "Stop"
            , buttonView VolumePlusButton "+"
            , buttonView VolumeMinusButton "-"
            , muteButtonView model.muteState
            ]


playPauseButtonView : PlayState -> Element Class variation Msg
playPauseButtonView state =
    case state of
        PlayingState ->
            buttonView PauseButton "Pause"

        PausedState ->
            buttonView PlayButton "Play"


muteButtonView : MuteState -> Element Class variation Msg
muteButtonView state =
    case state of
        MutedState ->
            buttonView UnmuteButton "Unmute"

        UnmutedState ->
            buttonView MuteButton "Mute"


buttonView : ButtonType -> String -> Element Class variation Msg
buttonView buttonType backupText =
    button (ButtonStyle buttonType)
        [ width (px 16)
        , height (px 16)
        , onClick (buttonMsg buttonType)
        ]
    <|
        text backupText


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
            FakeClicked


{-| This displays the last event that was handled and exists primarily so that
I could see real-time what was being handled and what wasn't.
-}
currentEventView : Maybe String -> Element style variation msg
currentEventView maybeEvent =
    Element.whenJust maybeEvent
        (\event -> text <| "Current event: " ++ event)



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
    | Replace
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

        Replace ->
            Encode.object
                [ "kind" => Encode.string "replace"
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
