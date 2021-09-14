module Die exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--
-- import Html.Attributes exposing (src)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Process
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (..)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dice : Dice
    , countdown : Int
    }


type alias Dice =
    { die1 : DieFace
    , die2 : DieFace
    }


type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Dice One One) 0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Countdown Dice
    | SleepComplete
    | NewFaces Dice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( { model | countdown = 7 }
            , Random.generate Countdown rollAllDice
            )

        Countdown dice ->
            let
                newCountdown =
                    model.countdown - 1
            in
            ( { model | dice = dice, countdown = newCountdown }
            , if newCountdown == 0 then
                Random.generate NewFaces rollAllDice

              else
                Process.sleep 100
                    |> Task.perform (\_ -> SleepComplete)
            )

        SleepComplete ->
            ( model, Random.generate Countdown rollAllDice )

        NewFaces dice ->
            ( Model dice 7
            , Cmd.none
            )


rollAllDice : Random.Generator Dice
rollAllDice =
    Random.map2 Dice rollDie rollDie


rollDie : Random.Generator DieFace
rollDie =
    Random.weighted ( 1, One )
        [ ( 1, Two )
        , ( 1, Three )
        , ( 1, Four )
        , ( 1, Five )
        , ( 1, Six )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);" ]
        [ getDieFaceSvg model.dice.die1
        , getDieFaceSvg model.dice.die2
        , br [] []
        , button [ style "background-color: #4CAF50; border: none; color: white; padding: 15px 32px; text-align: center; text-decoration: none; width: 100%; font-size: 16px;", onClick Roll ] [ text "Roll" ]
        ]


getDieFaceSvg : DieFace -> Svg msg
getDieFaceSvg dieFace =
    case dieFace of
        One ->
            makeSvgDice [ svgDot "60" "60" ]

        Two ->
            makeSvgDice [ svgDot "30" "30", svgDot "90" "90" ]

        Three ->
            makeSvgDice [ svgDot "30" "30", svgDot "60" "60", svgDot "90" "90" ]

        Four ->
            makeSvgDice [ svgDot "30" "30", svgDot "30" "90", svgDot "90" "30", svgDot "90" "90" ]

        Five ->
            makeSvgDice [ svgDot "30" "30", svgDot "30" "90", svgDot "90" "30", svgDot "90" "90", svgDot "60" "60" ]

        Six ->
            makeSvgDice [ svgDot "30" "30", svgDot "30" "90", svgDot "90" "30", svgDot "30" "60", svgDot "90" "60", svgDot "90" "90" ]


svgDot : String -> String -> Svg msg
svgDot dotX dotY =
    circle
        [ cx dotX
        , cy dotY
        , r "13"
        , fill "black"
        ]
        []


makeSvgDice : List (Svg msg) -> Svg msg
makeSvgDice circles =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , fill "red"
            , stroke "black"
            ]
            []
            :: circles
        )
