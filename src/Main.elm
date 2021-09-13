module Main exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--
-- import Html.Attributes exposing (src)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes exposing (..)



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
    ( Model (Dice One One)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Countdown ( Int, Dice )
    | NewFaces Dice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate Countdown ( 10, rollAllDice )
            )

        Countdown ( countdown, dice ) ->
            ( { model | dice = dice }
            , if countdown > 0 then
                Random.generate NewFaces rollAllDice

              else
                Random.generate Countdown ( countdown - 1, rollAllDice )
            )

        NewFaces newFace ->
            ( Model newFace
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
    div []
        [ getDieFaceSvg model.dice.die1
        , getDieFaceSvg model.dice.die2
        , br [] []
        , button [ onClick Roll ] [ text "Roll" ]
        ]


dieDotRadius : String
dieDotRadius =
    "13"


dieDotColor : String
dieDotColor =
    "red"


dieColor : String
dieColor =
    "green"


getDieFaceSvg : DieFace -> Svg msg
getDieFaceSvg dieFace =
    case dieFace of
        One ->
            makeSvgDice
                [ circle
                    [ cx "60"
                    , cy "60"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]

        Two ->
            makeSvgDice
                [ circle
                    [ cx "30"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]

        Three ->
            makeSvgDice
                [ circle
                    [ cx "30"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "60"
                    , cy "60"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]

        Four ->
            makeSvgDice
                [ circle
                    [ cx "30"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "30"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]

        Five ->
            makeSvgDice
                [ circle
                    [ cx "30"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "30"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "60"
                    , cy "60"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]

        Six ->
            makeSvgDice
                [ circle
                    [ cx "30"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "30"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "30"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "30"
                    , cy "60"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "60"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                , circle
                    [ cx "90"
                    , cy "90"
                    , r dieDotRadius
                    , fill dieDotColor
                    ]
                    []
                ]


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
            , fill "white"
            , stroke "black"
            ]
            []
            :: circles
        )
