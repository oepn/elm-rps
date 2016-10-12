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
        , ul [] <| map matchResult matches
        , div [] <| map throwSignButton signs
        ]


throwSignButton : Sign -> Html Msg
throwSignButton sign =
    button [ onClick <| Throw sign ] [ text (toString sign) ]


matchResult : Match -> Html msg
matchResult match =
    li []
        [ text <|
            toString (fst match)
                ++ " vs. "
                ++ toString (snd match)
                ++ " - "
                ++ (member match winningMatches ?: ( "You Win", "You Lose" ))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HELPERS


(?:) : Bool -> ( String, String ) -> String
(?:) bool ( trueString, falseString ) =
    if bool then
        trueString
    else
        falseString
