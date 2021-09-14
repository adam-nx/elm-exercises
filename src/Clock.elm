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
import Html.Events exposing (onClick)
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
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ ":" ++ millisecond) ]
        , button [ onClick ToggleClockState ] [ text pauseButtonText ]
        ]
