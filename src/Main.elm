module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr exposing (disabled, type', value)
import Html.Events exposing (..)
import List exposing (..)
import Random
import String


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL


type alias Model =
    { matches : List MatchSummary
    , matchLimit : Maybe Int
    }


type alias MatchSummary =
    ( Match, MatchResult )


type alias Match =
    ( Sign, Sign )


type Sign
    = Rock
    | Paper
    | Scissors


type MatchResult
    = Lose
    | Draw
    | Win


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing, Cmd.none )



-- UPDATE


type Msg
    = ThrowSign Sign
    | AddMatch Match
    | Reset
    | ChangeLimit (Maybe Int)


signs : List Sign
signs =
    [ Rock, Paper, Scissors ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ThrowSign sign ->
            ( model, randomSign sign )

        AddMatch match ->
            ( { model | matches = ( match, matchResult match ) :: model.matches }
            , Cmd.none
            )

        Reset ->
            ( { model | matches = [] }, Cmd.none )

        ChangeLimit limit ->
            ( { model | matchLimit = Maybe.map (max 0) limit }, Cmd.none )


randomSign : Sign -> Cmd Msg
randomSign sign =
    signs
        |> length
        |> Random.int 1
        |> Random.generate (makeMatch sign)


makeMatch : Sign -> Int -> Msg
makeMatch sign i =
    AddMatch ( sign, signs !! (i - 1) ? Rock )


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



-- VIEW


view : Model -> Html Msg
view { matches, matchLimit } =
    let
        stringMatchLimit : String
        stringMatchLimit =
            case matchLimit of
                Just limit ->
                    toString limit

                Nothing ->
                    ""

        matchCount : Int
        matchCount =
            length matches
    in
        div []
            [ h1 []
                [ text <|
                    "Matches ("
                        ++ toString (matchLimit ? 0 - matchCount)
                        ++ " remaining)"
                ]
            , input
                [ type' "number"
                , Attr.min "0"
                , value stringMatchLimit
                , onInput handleChangeLimit
                ]
                []
            , matchResultTotals matches
            , div [] <| map (signButton <| matchCount < matchLimit ? 0) signs
            , button [ onClick Reset ] [ text "Reset" ]
            , ul [] <| map matchResultEntry matches
            ]


handleChangeLimit : String -> Msg
handleChangeLimit =
    String.toInt >> Result.toMaybe >> ChangeLimit


signButton : Bool -> Sign -> Html Msg
signButton enabled sign =
    button
        [ disabled <| not enabled
        , onClick <| ThrowSign sign
        ]
        [ text (toString sign) ]


matchResultEntry : MatchSummary -> Html msg
matchResultEntry ( ( mySign, theirSign ), result ) =
    li []
        [ text <|
            toString mySign
                ++ " vs. "
                ++ toString theirSign
                ++ " - "
                ++ toString result
        ]


matchResultTotals : List MatchSummary -> Html msg
matchResultTotals matches =
    let
        updateTotals : MatchSummary -> ( Int, Int, Int ) -> ( Int, Int, Int )
        updateTotals ( _, result ) ( l, d, w ) =
            case result of
                Lose ->
                    ( l + 1, d, w )

                Draw ->
                    ( l, d + 1, w )

                Win ->
                    ( l, d, w + 1 )

        ( lose, draw, win ) =
            List.foldl updateTotals ( 0, 0, 0 ) matches
    in
        div []
            [ p [] [ text <| "Win: " ++ (toString win) ]
            , p [] [ text <| "Draw: " ++ (toString draw) ]
            , p [] [ text <| "Lose: " ++ (toString lose) ]
            ]



-- HELPERS


(?) : Maybe a -> a -> a
(?) maybe default =
    Maybe.withDefault default maybe


(!!) : List a -> Int -> Maybe a
(!!) list i =
    list |> Array.fromList |> Array.get i
