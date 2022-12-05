{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2022/day/5

-}


module Main exposing (main)

import Browser
import Html exposing (Html, a, div, form, h2, text, textarea)
import Html.Attributes exposing (action, class, placeholder, required, rows, style)
import Html.Events exposing (onClick, onInput)
import List exposing (drop, filterMap, foldl, head, map, reverse, tail, take)
import Maybe exposing (Maybe)
import String exposing (concat, fromList, left, length, lines, right, split, toInt, toList, trim)
import Tuple exposing (first)


type alias Model =
    { inputString : String
    , solution1 : String
    , solution2 : String
    }


initialModel : Model
initialModel =
    { inputString = ""
    , solution1 = ""
    , solution2 = ""
    }


type Msg
    = LoadInputFromForm String
    | LoadInputFromCache
    | FindSolutions


type alias Stacks =
    List (List Char)


type ModelNumber
    = Model9000
    | Model9001


parseInput : String -> ( Stacks, String )
parseInput input =
    case split "\n\n" input of
        stackInput :: rearrangementInput :: [] ->
            ( parseStacks stackInput, rearrangementInput )

        _ ->
            ( [ [] ], "" )


transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
            (x :: heads) :: transpose (xs :: tails)


parseStack : List Char -> Maybe (List Char)
parseStack i =
    case i of
        ' ' :: _ ->
            Nothing

        _ :: stackContent ->
            Just (toList <| trim <| fromList stackContent)

        _ ->
            Nothing


parseStacks : String -> Stacks
parseStacks i =
    lines i
        |> map toList
        |> transpose
        |> map reverse
        |> map parseStack
        |> filterMap identity


performRearrangement : String -> ( Stacks, ModelNumber ) -> ( Stacks, ModelNumber )
performRearrangement rearrangement ( stacks, model ) =
    case split " " rearrangement of
        _ :: x :: _ :: y :: _ :: z :: _ ->
            case ( toInt x, toInt y, toInt z ) of
                ( Just numberToRemove, Just from, Just to ) ->
                    let
                        ( stacksAfterPickUp, pickedUp ) =
                            pickUpCrates model stacks numberToRemove from
                    in
                    ( placeCrates stacksAfterPickUp pickedUp to, model )

                _ ->
                    ( stacks, model )

        _ ->
            ( stacks, model )


pickUpCrates : ModelNumber -> Stacks -> Int -> Int -> ( Stacks, List Char )
pickUpCrates model stacks numberOfCrates column =
    let
        leftStacks =
            take (column - 1) stacks

        rest =
            drop (column - 1) stacks
    in
    case head rest of
        Nothing ->
            ( stacks, [ '?' ] )

        Just fromColumn ->
            let
                currentAsString =
                    trim (fromList fromColumn)

                pickedUp =
                    case model of
                        Model9000 ->
                            reverse <| toList <| right numberOfCrates currentAsString

                        Model9001 ->
                            toList <| right numberOfCrates currentAsString

                remaining =
                    left (length currentAsString - numberOfCrates) currentAsString
            in
            case tail rest of
                Nothing ->
                    ( leftStacks ++ [ toList remaining ], pickedUp )

                Just rightStacks ->
                    ( leftStacks ++ toList remaining :: rightStacks, pickedUp )


placeCrates : Stacks -> List Char -> Int -> Stacks
placeCrates stacks pickedUp to =
    case stacks of
        s :: ss ->
            if to == 1 then
                (s ++ pickedUp) :: ss

            else
                s :: placeCrates ss pickedUp (to - 1)

        _ ->
            []


findTopCrates : ModelNumber -> Stacks -> String -> String
findTopCrates m s r =
    lines r
        |> foldl performRearrangement ( s, m )
        |> first
        |> map reverse
        |> map fromList
        |> map trim
        |> map (left 1)
        |> concat


update : Msg -> Model -> Model
update msg model =
    case msg of
        LoadInputFromForm text ->
            { model | inputString = text }

        LoadInputFromCache ->
            { model | inputString = inputString }

        FindSolutions ->
            let
                ( stacks, rearrangements ) =
                    parseInput model.inputString
            in
            { model
                | solution1 = findTopCrates Model9000 stacks rearrangements
                , solution2 = findTopCrates Model9001 stacks rearrangements
            }




view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "w-1/2 mx-auto" ]
                [ h2 [ class "text-xl" ] [ text "Day5: Supply Stacks" ]
                ]
            , div [ class "flex space-x-8 justify-center" ]
                [ a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick LoadInputFromCache
                    ]
                    [ text "Load data" ]
                , a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick FindSolutions
                    ]
                    [ text "Find solutions" ]
                ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "form-control block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
                        , rows 3
                        , placeholder "Paste input text here"
                        , required True
                        , onInput LoadInputFromForm
                        ]
                        [ text model.inputString ]
                    ]
                , div [ class "flex space-x-8 justify-center" ]
                    [ div [ class "textarea_label" ] [ text "First task solution: " ]
                    , text <| model.solution1
                    , div [ class "textarea_label" ] [ text "Second task solution: " ]
                    , text <| model.solution2
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


inputString : String
inputString =
    """[V]     [B]                     [F]
[N] [Q] [W]                 [R] [B]
[F] [D] [S]     [B]         [L] [P]
[S] [J] [C]     [F] [C]     [D] [G]
[M] [M] [H] [L] [P] [N]     [P] [V]
[P] [L] [D] [C] [T] [Q] [R] [S] [J]
[H] [R] [Q] [S] [V] [R] [V] [Z] [S]
[J] [S] [N] [R] [M] [T] [G] [C] [D]
 1   2   3   4   5   6   7   8   9 

move 1 from 8 to 4
move 1 from 7 to 8
move 1 from 6 to 3
move 2 from 6 to 5
move 8 from 5 to 1
move 5 from 3 to 8
move 1 from 7 to 8
move 8 from 1 to 2
move 3 from 3 to 9
move 13 from 8 to 7
move 2 from 1 to 2
move 1 from 6 to 2
move 2 from 1 to 7
move 4 from 4 to 2
move 10 from 9 to 4
move 7 from 4 to 1
move 1 from 6 to 7
move 2 from 4 to 5
move 1 from 5 to 2
move 1 from 5 to 8
move 3 from 1 to 5
move 2 from 4 to 6
move 2 from 6 to 4
move 2 from 4 to 5
move 5 from 1 to 5
move 1 from 9 to 5
move 1 from 8 to 5
move 14 from 2 to 6
move 12 from 7 to 4
move 4 from 6 to 7
move 3 from 6 to 4
move 4 from 4 to 9
move 2 from 4 to 6
move 2 from 9 to 3
move 9 from 4 to 3
move 2 from 1 to 6
move 5 from 7 to 3
move 4 from 7 to 8
move 2 from 6 to 7
move 3 from 6 to 7
move 10 from 5 to 8
move 8 from 8 to 9
move 10 from 9 to 7
move 12 from 7 to 5
move 1 from 1 to 5
move 3 from 7 to 2
move 10 from 3 to 7
move 6 from 5 to 7
move 2 from 6 to 1
move 12 from 2 to 7
move 3 from 3 to 1
move 1 from 6 to 5
move 10 from 5 to 7
move 3 from 3 to 4
move 34 from 7 to 1
move 2 from 6 to 9
move 1 from 6 to 3
move 3 from 1 to 3
move 1 from 7 to 3
move 3 from 3 to 6
move 1 from 4 to 3
move 22 from 1 to 6
move 2 from 9 to 7
move 2 from 4 to 9
move 12 from 6 to 8
move 1 from 7 to 6
move 4 from 8 to 2
move 1 from 7 to 1
move 6 from 8 to 9
move 1 from 2 to 5
move 1 from 2 to 7
move 13 from 1 to 2
move 2 from 3 to 1
move 4 from 7 to 5
move 3 from 9 to 4
move 1 from 1 to 8
move 4 from 5 to 2
move 12 from 6 to 2
move 3 from 1 to 3
move 1 from 4 to 1
move 9 from 8 to 5
move 6 from 5 to 7
move 2 from 4 to 5
move 5 from 9 to 6
move 5 from 3 to 7
move 30 from 2 to 6
move 11 from 7 to 9
move 36 from 6 to 3
move 10 from 9 to 3
move 1 from 6 to 5
move 3 from 5 to 2
move 2 from 5 to 2
move 28 from 3 to 4
move 6 from 4 to 1
move 1 from 2 to 3
move 2 from 5 to 2
move 6 from 1 to 7
move 1 from 1 to 6
move 6 from 3 to 5
move 6 from 7 to 2
move 1 from 6 to 4
move 2 from 2 to 6
move 8 from 2 to 1
move 3 from 2 to 4
move 2 from 3 to 4
move 4 from 3 to 4
move 1 from 6 to 1
move 2 from 1 to 8
move 1 from 6 to 4
move 1 from 9 to 3
move 2 from 5 to 4
move 1 from 8 to 7
move 1 from 7 to 9
move 1 from 3 to 5
move 1 from 8 to 6
move 34 from 4 to 9
move 13 from 9 to 8
move 1 from 8 to 2
move 1 from 2 to 1
move 4 from 5 to 1
move 9 from 8 to 7
move 11 from 1 to 3
move 1 from 4 to 2
move 1 from 6 to 7
move 1 from 9 to 4
move 1 from 4 to 1
move 1 from 5 to 3
move 5 from 7 to 8
move 1 from 2 to 5
move 1 from 5 to 1
move 21 from 9 to 5
move 19 from 3 to 4
move 17 from 4 to 6
move 2 from 8 to 4
move 2 from 6 to 8
move 2 from 6 to 9
move 2 from 7 to 6
move 1 from 4 to 9
move 6 from 5 to 6
move 1 from 9 to 8
move 8 from 5 to 7
move 15 from 6 to 2
move 1 from 9 to 7
move 2 from 1 to 6
move 3 from 4 to 7
move 1 from 1 to 6
move 3 from 5 to 4
move 2 from 5 to 6
move 2 from 4 to 1
move 13 from 7 to 8
move 2 from 6 to 4
move 3 from 2 to 4
move 2 from 7 to 6
move 5 from 4 to 6
move 4 from 2 to 6
move 1 from 1 to 9
move 18 from 8 to 3
move 1 from 4 to 5
move 1 from 2 to 7
move 15 from 3 to 1
move 1 from 5 to 1
move 3 from 3 to 4
move 1 from 5 to 4
move 1 from 5 to 6
move 1 from 6 to 8
move 2 from 8 to 2
move 3 from 1 to 8
move 6 from 2 to 8
move 1 from 7 to 6
move 12 from 8 to 5
move 2 from 9 to 6
move 6 from 1 to 5
move 9 from 5 to 3
move 1 from 2 to 8
move 20 from 6 to 9
move 3 from 6 to 7
move 1 from 7 to 1
move 7 from 3 to 4
move 2 from 7 to 2
move 1 from 8 to 7
move 8 from 4 to 1
move 11 from 1 to 7
move 10 from 7 to 6
move 2 from 4 to 9
move 21 from 9 to 3
move 6 from 5 to 9
move 6 from 3 to 2
move 1 from 4 to 5
move 1 from 7 to 9
move 8 from 3 to 2
move 9 from 2 to 1
move 14 from 1 to 6
move 1 from 1 to 7
move 4 from 3 to 8
move 3 from 8 to 7
move 5 from 7 to 4
move 3 from 6 to 9
move 2 from 3 to 7
move 3 from 5 to 6
move 1 from 5 to 6
move 2 from 7 to 9
move 1 from 8 to 3
move 22 from 6 to 5
move 3 from 9 to 4
move 3 from 6 to 1
move 5 from 4 to 6
move 9 from 2 to 8
move 4 from 6 to 1
move 1 from 3 to 2
move 1 from 2 to 3
move 6 from 8 to 1
move 2 from 4 to 3
move 10 from 1 to 7
move 2 from 8 to 7
move 1 from 9 to 6
move 4 from 3 to 5
move 1 from 8 to 3
move 4 from 9 to 8
move 1 from 4 to 3
move 1 from 3 to 8
move 3 from 7 to 6
move 1 from 1 to 5
move 10 from 5 to 9
move 5 from 6 to 4
move 5 from 8 to 5
move 4 from 9 to 8
move 3 from 3 to 9
move 2 from 8 to 6
move 5 from 7 to 5
move 1 from 4 to 1
move 1 from 1 to 2
move 2 from 8 to 6
move 1 from 2 to 1
move 1 from 7 to 2
move 1 from 1 to 5
move 28 from 5 to 9
move 3 from 6 to 1
move 1 from 6 to 9
move 1 from 2 to 9
move 2 from 1 to 2
move 2 from 7 to 5
move 1 from 7 to 5
move 1 from 2 to 5
move 3 from 1 to 9
move 1 from 5 to 8
move 15 from 9 to 2
move 11 from 9 to 4
move 11 from 4 to 7
move 2 from 4 to 1
move 7 from 7 to 8
move 1 from 1 to 4
move 20 from 9 to 1
move 2 from 7 to 8
move 1 from 4 to 6
move 1 from 6 to 2
move 2 from 7 to 5
move 1 from 9 to 6
move 1 from 4 to 9
move 4 from 5 to 2
move 1 from 6 to 8
move 1 from 4 to 9
move 11 from 8 to 3
move 1 from 1 to 9
move 1 from 5 to 9
move 1 from 2 to 6
move 4 from 9 to 8
move 4 from 8 to 7
move 10 from 1 to 6
move 7 from 1 to 5
move 8 from 3 to 4
move 2 from 3 to 5
move 3 from 7 to 4
move 1 from 4 to 5
move 2 from 1 to 6
move 9 from 2 to 6
move 1 from 7 to 9
move 1 from 3 to 2
move 7 from 4 to 3
move 3 from 3 to 7
move 5 from 2 to 3
move 1 from 1 to 9
move 2 from 2 to 7
move 1 from 4 to 6
move 3 from 5 to 6
move 4 from 7 to 6
move 1 from 7 to 4
move 1 from 4 to 7
move 1 from 2 to 8
move 1 from 7 to 1
move 27 from 6 to 2
move 1 from 4 to 1
move 7 from 5 to 7
move 1 from 4 to 1
move 1 from 8 to 3
move 3 from 7 to 3
move 2 from 1 to 6
move 2 from 9 to 1
move 18 from 2 to 1
move 2 from 7 to 5
move 12 from 3 to 4
move 1 from 5 to 6
move 3 from 6 to 1
move 24 from 1 to 8
move 9 from 2 to 4
move 3 from 2 to 1
move 2 from 6 to 3
move 1 from 6 to 9
move 1 from 5 to 6
move 1 from 6 to 2
move 1 from 1 to 7
move 1 from 2 to 1
move 1 from 1 to 2
move 3 from 7 to 2
move 2 from 1 to 4
move 8 from 4 to 5
move 22 from 8 to 1
move 1 from 8 to 1
move 13 from 4 to 1
move 1 from 8 to 5
move 3 from 3 to 1
move 1 from 2 to 7
move 38 from 1 to 6
move 27 from 6 to 1
move 2 from 2 to 9
move 3 from 9 to 8
move 2 from 8 to 6
move 1 from 8 to 3
move 1 from 2 to 1
move 1 from 3 to 6
move 1 from 2 to 3
move 1 from 7 to 6
move 7 from 6 to 3
move 20 from 1 to 4
move 6 from 1 to 6
move 17 from 4 to 7
move 3 from 6 to 5
move 14 from 7 to 9
move 8 from 5 to 7
move 3 from 1 to 6
move 3 from 3 to 1
move 2 from 4 to 1
move 4 from 5 to 1
move 9 from 6 to 2
move 3 from 6 to 4
move 4 from 7 to 8
move 4 from 1 to 6
move 2 from 3 to 1
move 6 from 6 to 7
move 4 from 8 to 7
move 4 from 2 to 1
move 4 from 2 to 3
move 4 from 9 to 5
move 8 from 9 to 5
move 1 from 9 to 5
move 1 from 2 to 1
move 16 from 7 to 2
move 10 from 2 to 9
move 11 from 9 to 8
move 4 from 3 to 5
move 3 from 1 to 4
move 13 from 5 to 7
move 10 from 8 to 5
move 2 from 1 to 5
move 11 from 7 to 4
move 2 from 3 to 6
move 3 from 7 to 6
move 1 from 3 to 2
move 1 from 1 to 8
move 2 from 8 to 4
move 3 from 1 to 2
move 4 from 6 to 1
move 7 from 1 to 9
move 1 from 6 to 7
move 2 from 5 to 8
move 1 from 2 to 9
move 1 from 7 to 8
move 5 from 5 to 8
move 1 from 2 to 3
move 4 from 2 to 5
move 17 from 4 to 1
move 10 from 5 to 9
move 2 from 4 to 2
move 2 from 4 to 1
move 1 from 4 to 9
move 1 from 3 to 7
move 1 from 7 to 8
move 12 from 9 to 2
move 1 from 2 to 4
move 1 from 4 to 1
move 1 from 1 to 9
move 1 from 8 to 1
move 8 from 8 to 3
move 2 from 5 to 1
move 3 from 1 to 9
move 1 from 2 to 6
move 4 from 3 to 7
move 1 from 7 to 6
move 10 from 9 to 2
move 1 from 5 to 9
move 1 from 9 to 3
move 17 from 1 to 6
move 2 from 1 to 2
move 11 from 6 to 7
move 2 from 2 to 9
move 2 from 9 to 5
move 12 from 7 to 9
move 20 from 2 to 7
move 5 from 9 to 5
move 21 from 7 to 1
move 2 from 6 to 4
move 11 from 1 to 4
move 5 from 4 to 6
move 1 from 7 to 8
move 5 from 9 to 3
move 5 from 2 to 8
move 3 from 9 to 3
move 2 from 8 to 7
move 2 from 1 to 7
move 10 from 6 to 3
move 1 from 2 to 6
move 2 from 8 to 5
move 1 from 6 to 5
move 2 from 4 to 9
move 1 from 4 to 5
move 8 from 1 to 6
move 4 from 4 to 8
move 6 from 8 to 4
move 21 from 3 to 9
move 5 from 9 to 2
move 4 from 7 to 9
move 22 from 9 to 3
move 9 from 6 to 4
move 2 from 2 to 6
move 2 from 2 to 1
move 2 from 5 to 7
move 7 from 5 to 4
move 22 from 4 to 2
move 2 from 5 to 4
move 16 from 2 to 5
move 2 from 6 to 2
move 13 from 3 to 4
move 5 from 5 to 7
move 15 from 4 to 7
move 3 from 2 to 3
move 3 from 2 to 5
move 1 from 1 to 2
move 1 from 2 to 4
move 6 from 5 to 9
move 4 from 3 to 6
move 2 from 5 to 9
move 1 from 2 to 7
move 1 from 1 to 9
move 2 from 4 to 5
move 19 from 7 to 8
move 1 from 6 to 5
move 1 from 5 to 1
move 1 from 9 to 4
move 5 from 8 to 1
move 3 from 8 to 1
move 7 from 5 to 6
move 3 from 7 to 1
move 1 from 2 to 5
move 4 from 9 to 8
move 2 from 5 to 6
move 10 from 1 to 4
move 1 from 7 to 2
move 6 from 3 to 4
move 9 from 4 to 3
move 2 from 2 to 8
move 2 from 9 to 5
move 5 from 8 to 3
move 1 from 1 to 5
move 2 from 5 to 6
move 1 from 1 to 7
move 2 from 9 to 7
move 8 from 4 to 7
move 3 from 3 to 9
move 4 from 6 to 3
move 1 from 5 to 3
move 1 from 7 to 2
move 1 from 2 to 1
move 1 from 6 to 5
move 1 from 5 to 2
move 10 from 7 to 4
move 10 from 4 to 1
move 10 from 1 to 8
move 1 from 9 to 6
move 1 from 1 to 4
move 11 from 8 to 1
move 2 from 9 to 5
move 5 from 6 to 3
move 1 from 3 to 8
move 4 from 1 to 3
move 5 from 3 to 8
move 1 from 4 to 7
move 1 from 7 to 2
move 13 from 3 to 5
move 2 from 2 to 1
move 4 from 3 to 1
move 4 from 5 to 6
move 3 from 6 to 2
move 4 from 5 to 4
move 8 from 8 to 7
move 1 from 3 to 9"""
