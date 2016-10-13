module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import List exposing (..)
import Random


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
    = ThrowSign Sign
    | AddMatch Match


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
        ThrowSign sign ->
            ( model, randomSign sign )

        AddMatch match ->
            ( { model | matches = match :: model.matches }, Cmd.none )


randomSign : Sign -> Cmd Msg
randomSign sign =
    signs
        |> length
        |> Random.int 1
        |> Random.generate (makeMatch sign)


makeMatch : Sign -> Int -> Msg
makeMatch sign i =
    AddMatch ( sign, signs !! (i - 1) ? Rock )



-- VIEW


view : Model -> Html Msg
view { matches } =
    div []
        [ h1 [] [ text "Matches" ]
        , div [] <| map signButton signs
        , ul [] <| map matchResultEntry matches
        ]


signButton : Sign -> Html Msg
signButton sign =
    button [ onClick <| ThrowSign sign ] [ text (toString sign) ]


matchResultEntry : Match -> Html msg
matchResultEntry match =
    let
        ( mySign, theirSign ) =
            match
    in
        li []
            [ text <|
                toString mySign
                    ++ " vs. "
                    ++ toString theirSign
                    ++ " - "
                    ++ toString (matchResult match)
            ]


matchResult : Match -> MatchResult
matchResult ( mySign, theirSign ) =
    let
        winningMatches =
            [ ( Rock, Scissors )
            , ( Paper, Rock )
            , ( Scissors, Paper )
            ]
    in
        if (member ( mySign, theirSign ) winningMatches) then
            Win
        else if (mySign == theirSign) then
            Draw
        else
            Lose



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HELPERS


(?) : Maybe a -> a -> a
(?) maybe default =
    Maybe.withDefault default maybe


(!!) : List a -> Int -> Maybe a
(!!) list i =
    list |> Array.fromList |> Array.get i
