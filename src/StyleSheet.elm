module StyleSheet exposing (Style(..), ButtonType(..), stylesheet)

import Color
import Style exposing (StyleSheet)
import Style.Border as Border
import Style.Background as Background
import Style.Font as Font
import Style.Color as Color


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
                , Color.background transparent
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


transparent : Color.Color
transparent =
    Color.rgba 0 0 0 0
