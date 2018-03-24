module StyleSheet exposing (Class(..), ButtonType(..), stylesheet)

import Color
import Style exposing (Style, StyleSheet)
import Style.Border as Border
import Style.Background as Background
import Style.Font as Font
import Style.Color as Color


{-| These are the different classes that any element can have. They're
statically checked at compile time, and it is impossible to have an element
without a class.
-}
type Class
    = DefaultStyle
    | BodyStyle
    | HeaderStyle
    | PlayerStyle
    | VideoStyle
    | ButtonStyle ButtonType


{-| Because there are many buttons, but they are all styled similarly, we let
ButtonStyle take a second argument for what kind of button it is. This way, the
buttons can be easily grouped together, and the compiler can ensure that we
have not missed any cases (see `buttonStyle` below).
-}
type ButtonType
    = PlayButton
    | PauseButton
    | StopButton
    | VolumePlusButton
    | VolumeMinusButton
    | MuteButton
    | UnmuteButton
    | ReplayButton


{-| This is analogous to a CSS file: it stores all of the style information for
all of the elements. Note that specific size information and positioning
information is left to the view function in our main file.
-}
stylesheet : StyleSheet Class variation
stylesheet =
    Style.styleSheet
        [ Style.style DefaultStyle []
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


{-| Buttons are all styled very similarly, so we use a function to construct
the style for each button type. Note that with CSS, this would be done through
inheritance, but there is no inheritance here.

Note also that, if we didn't have the extra `ButtonType` type on buttons, but
instead used lots of individual `Class` values, the compiler would expect us to
have background positions for every class, even ones that we'd never call
`buttonStyle` on. We'd be forced to have a default case, which would be
unhelpful and would eliminate Elm's static check that we have covered each
case.

-}
buttonStyle : ButtonType -> Style Class variation
buttonStyle buttonType =
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
    in
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


transparent : Color.Color
transparent =
    Color.rgba 0 0 0 0
