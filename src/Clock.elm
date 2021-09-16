module Clock exposing (..)

-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, viewBox, x1, x2, y1, y2)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , clockState : ClockState
    }


type ClockState
    = Paused
    | Unpaused


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) Unpaused
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleClockState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ToggleClockState ->
            ( { model | clockState = flipClockState model.clockState }, Cmd.none )


flipClockState : ClockState -> ClockState
flipClockState clockState =
    case clockState of
        Paused ->
            Unpaused

        Unpaused ->
            Paused



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.clockState of
        Unpaused ->
            Time.every 1 Tick

        Paused ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        millisecond =
            String.fromInt (Time.toMillis model.zone model.time)

        pauseButtonText =
            case model.clockState of
                Paused ->
                    "GO!"

                Unpaused ->
                    "STOP"
    in
    div
        [ style "position" "fixed"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        ]
        [ svgClock hour minute second millisecond
        , button [ onClick ToggleClockState ] [ text pauseButtonText ]
        ]


clockRadius : Float
clockRadius =
    45.0


milliCoordinates : String -> ( String, String )
milliCoordinates milli =
    handCoordinates (angleFromMilli milli) 0.4


secondCoordinates : String -> ( String, String )
secondCoordinates second =
    handCoordinates (angleFromMinute second) 0.6


minuteCoordinates : String -> ( String, String )
minuteCoordinates minute =
    handCoordinates (angleFromMinute minute) 0.8


hourCoordinates : String -> ( String, String )
hourCoordinates hour =
    handCoordinates (angleFromHour hour) 0.95


handCoordinates : Float -> Float -> ( String, String )
handCoordinates angle handLength =
    let
        ( circlePositionX, circlePositionY ) =
            unitCirclePosition angle (clockRadius * handLength)

        positionX =
            String.fromFloat circlePositionX

        positionY =
            String.fromFloat circlePositionY
    in
    ( positionX, positionY )


svgClock : String -> String -> String -> String -> Svg msg
svgClock hour minute second milli =
    svg
        [ viewBox "0 0 100 100" ]
        [ Svg.circle
            [ fill "yellow"
            , stroke "black"
            , cx "50"
            , cy "50"
            , r (String.fromFloat clockRadius)
            ]
            []
        , svgHand (hourCoordinates hour) "black"
        , svgHand (minuteCoordinates minute) "red"
        , svgHand (secondCoordinates second) "pink"
        , svgHand (milliCoordinates milli) "magenta"
        , Svg.circle
            [ fill "black"
            , stroke "black"
            , cx "50"
            , cy "50"
            , r "2"
            ]
            []
        ]


svgHand : ( String, String ) -> String -> Svg msg
svgHand ( positionX, positionY ) color =
    Svg.line [ x1 "50", y1 "50", x2 positionX, y2 positionY, stroke color ] []


unitCirclePosition : Float -> Float -> ( Float, Float )
unitCirclePosition angle scale =
    ( (sin (degrees angle) * scale) + 50
    , negate (cos (degrees angle) * scale) + 50
    )


angleFromHour : String -> Float
angleFromHour =
    angleFromNumber 12.0


angleFromMinute : String -> Float
angleFromMinute =
    angleFromNumber 60.0


angleFromMilli : String -> Float
angleFromMilli =
    angleFromNumber 1000.0


angleFromNumber : Float -> String -> Float
angleFromNumber scale numberString =
    case String.toFloat numberString of
        Just number ->
            (number / scale) * 360.0

        Nothing ->
            0.0
