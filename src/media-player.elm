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
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
    = Event String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Event event ->
            ( { model | currentEvent = Just event }, Cmd.none )



-- SUBSCRIPTIONS


port videoEvents : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toMsg value =
            Decode.decodeValue decodeEvent value
                |> Result.withDefault (Event "error")
    in
        videoEvents toMsg


decodeEvent : Decode.Decoder Msg
decodeEvent =
    Decode.index 0 Decode.string
        |> Decode.map Event



-- VIEW


view : Model -> Html Msg
view model =
    body []
        [ h1 [] [ text "Sample Media Player using HTML5's Media API" ]
        , div [ id "media-player" ]
            [ video [ id "media-video", autoplay True ]
                [ source [ src "videos/big-buck-bunny_trailer.webm", type_ "video/mp4" ] []
                ]
            , div [ id "media-controls" ]
                [ progress [ id "progress-bar", Attr.min "0", Attr.max "100", value "0" ] [ text "played" ]
                , button [ id "replay-button", class "replay", title "replay" ] [ text "Replay" ]
                , button [ id "play-pause-button", class "play", title "play" ] [ text "Play" ]
                , button [ id "stop-button", class "stop", title "stop" ] [ text "Stop" ]
                , button [ id "volume-inc-button", class "volume-plus", title "increase volume" ] [ text "Increase Volume" ]
                , button [ id "volume-dec-button", class "volume-minus", title "decrease volume" ] [ text "Decrease Volume" ]
                , button [ id "mute-button", class "mute", title "mute" ] [ text "Mute" ]
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
