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
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, viewBox, width, x1, x2, y1, y2)
import Task
import Time



-- MAIN


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
            let
                newClockState =
                    case model.clockState of
                        Paused ->
                            Unpaused

                        Unpaused ->
                            Paused
            in
            ( { model | clockState = newClockState }, Cmd.none )



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
                    "STOP!!!"
    in
    div
        [ style "position" "fixed"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        ]
        [ button [ onClick ToggleClockState ] [ text pauseButtonText ]
        , svgClock hour minute second millisecond
        ]


clockRadius =
    45.0


milliCoordinates : String -> Float -> ( String, String )
milliCoordinates milli length =
    let
        ( circlePositionX, circlePositionY ) =
            unitCirclePosition (angleFromMilli milli) (clockRadius * length)

        positionX =
            String.fromFloat circlePositionX

        positionY =
            String.fromFloat circlePositionY
    in
    ( positionX, positionY )


minuteCoordinates : String -> Float -> ( String, String )
minuteCoordinates minute length =
    let
        ( circlePositionX, circlePositionY ) =
            unitCirclePosition (angleFromMinute minute) (clockRadius * length)

        positionX =
            String.fromFloat circlePositionX

        positionY =
            String.fromFloat circlePositionY
    in
    ( positionX, positionY )


hourCoordinates : String -> ( String, String )
hourCoordinates hour =
    let
        ( circlePositionX, circlePositionY ) =
            unitCirclePosition (angleFromHour hour) (clockRadius * 0.95)

        positionX =
            String.fromFloat circlePositionX

        positionY =
            String.fromFloat circlePositionY
    in
    ( positionX, positionY )


svgClock : String -> String -> String -> String -> Svg msg
svgClock hour minute second milli =
    let
        ( hourPositionX, hourPositionY ) =
            hourCoordinates hour

        ( minutePositionX, minutePositionY ) =
            minuteCoordinates minute 0.8

        ( secondPositionX, secondPositionY ) =
            minuteCoordinates second 0.6

        ( milliPositionX, milliPositionY ) =
            milliCoordinates milli 0.4
    in
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
        , Svg.line
            [ x1 "50"
            , y1 "50"
            , x2 hourPositionX
            , y2 hourPositionY
            , stroke "black"
            ]
            []
        , Svg.line
            [ x1 "50"
            , y1 "50"
            , x2 minutePositionX
            , y2 minutePositionY
            , stroke "red"
            ]
            []
        , Svg.line
            [ x1 "50"
            , y1 "50"
            , x2 milliPositionX
            , y2 milliPositionY
            , stroke "green"
            ]
            []
        , Svg.line
            [ x1 "50"
            , y1 "50"
            , x2 secondPositionX
            , y2 secondPositionY
            , stroke "purple"
            ]
            []
        , Svg.circle
            [ fill "black"
            , stroke "black"
            , cx "50"
            , cy "50"
            , r "2"
            ]
            []
        ]


unitCirclePosition : Float -> Float -> ( Float, Float )
unitCirclePosition angle scale =
    ( (sin (degrees angle) * scale) + 50
    , negate (cos (degrees angle) * scale) + 50
    )


angleFromHour : String -> Float
angleFromHour hourString =
    case String.toFloat hourString of
        Just hour ->
            (hour / 12.0) * 360.0

        Nothing ->
            0.0


angleFromMinute : String -> Float
angleFromMinute minuteString =
    case String.toFloat minuteString of
        Just minute ->
            (minute / 60.0) * 360.0

        Nothing ->
            0.0


angleFromMilli : String -> Float
angleFromMilli milliString =
    case String.toFloat milliString of
        Just milli ->
            (milli / 1000.0) * 360.0

        Nothing ->
            0.0
