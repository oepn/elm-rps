module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import List exposing (..)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { matches : List Match }


type alias Match =
    ( Sign, Sign )


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )



-- UPDATE


type Msg
    = Throw Sign


type Sign
    = Rock
    | Paper
    | Scissors


type MatchResult
    = Lose
    | Draw
    | Win


signs : List Sign
signs =
    [ Rock, Paper, Scissors ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Throw sign ->
            ( { model | matches = ( sign, Rock ) :: model.matches }, Cmd.none )


winningMatches : List Match
winningMatches =
    [ ( Rock, Scissors )
    , ( Paper, Rock )
    , ( Scissors, Paper )
    ]



-- VIEW


view : Model -> Html Msg
view { matches } =
    div []
        [ h1 [] [ text "Matches" ]
        , div [] <| map throwSignButton signs
        , ul [] <| map matchResultEntry matches
        ]


throwSignButton : Sign -> Html Msg
throwSignButton sign =
    button [ onClick <| Throw sign ] [ text (toString sign) ]


matchResultEntry : Match -> Html msg
matchResultEntry match =
    li []
        [ text <|
            toString (fst match)
                ++ " vs. "
                ++ toString (snd match)
                ++ " - "
                ++ toString (matchResult match)
        ]


matchResult : Match -> MatchResult
matchResult ( sign, sign' ) =
    if (member ( sign, sign' ) winningMatches) then
        Win
    else if (sign == sign') then
        Draw
    else
        Lose



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
