port module Main exposing (..)

import Html exposing (a)
import List.Extra as List
import Platform exposing (Program)
import Set


port submit : String -> Cmd msg



-- ここを書き換える。 solveN : List String -> String な関数を作れば基本大丈夫。


solveA : List String -> String
solveA inputs =
    case Maybe.map (List.filterMap String.toInt << String.split " ") <| List.head <| inputs of
        Just (a :: b :: []) ->
            if modBy 2 (a * b) == 0 then
                "Even"

            else
                "Odd"

        _ ->
            "fail"


solveB : List String -> String
solveB inputs =
    String.fromInt <|
        Maybe.withDefault 0 <|
            Maybe.map (String.length << String.filter ((==) '1')) <|
                List.head <|
                    inputs


solveC : List String -> String
solveC inputs =
    let
        loop : List Int -> Int -> Int
        loop nums c =
            let
                isEven num =
                    modBy 2 num == 0

                divide2 =
                    List.map (\x -> x // 2)
            in
            if List.all isEven nums then
                loop (divide2 nums) c + 1

            else
                c
    in
    case inputs of
        _ :: numStrs :: [] ->
            let
                nums =
                    List.filterMap String.toInt <| String.split " " numStrs
            in
            String.fromInt <| loop nums 0

        _ ->
            "fail"


solveD : List String -> String
solveD inputs =
    case List.filterMap String.toInt <| inputs of
        a :: b :: c :: x :: [] ->
            String.fromInt <|
                List.length <|
                    List.filter ((==) x) <|
                        List.lift3
                            (\aa bb cc -> aa * 500 + bb * 100 + cc * 50)
                            (List.range 0 a)
                            (List.range 0 b)
                            (List.range 0 c)

        _ ->
            "fail"


solveE : List String -> String
solveE inputs =
    let
        sumDigit n =
            String.fromInt n
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap String.toInt
                |> List.sum
    in
    case Maybe.map (String.split " " >> List.filterMap String.toInt) <| List.head inputs of
        Just (n :: a :: b :: []) ->
            String.fromInt <|
                (List.range 1 n
                    |> List.filterMap
                        (\x ->
                            let
                                sum =
                                    sumDigit x
                            in
                            if a <= sum && sum <= b then
                                Just x

                            else
                                Nothing
                        )
                    |> List.sum
                )

        _ ->
            "fail"


solveF : List String -> String
solveF inputs =
    case inputs of
        _ :: aStrs :: [] ->
            let
                isEven num =
                    modBy 2 num == 0

                isOdd =
                    not << isEven

                cards =
                    (String.split " " >> List.filterMap String.toInt >> List.sort >> List.reverse >> List.indexedMap Tuple.pair) aStrs

                aliceCard =
                    cards
                        |> List.filterMap
                            (\( index, x ) ->
                                if isEven index then
                                    Just x

                                else
                                    Nothing
                            )

                bobCard =
                    cards
                        |> List.filterMap
                            (\( index, x ) ->
                                if isOdd index then
                                    Just x

                                else
                                    Nothing
                            )
            in
            String.fromInt <| List.sum aliceCard - List.sum bobCard

        _ ->
            "fail"


solveG : List String -> String
solveG inputs =
    case inputs of
        _ :: numStrs ->
            let
                nums =
                    numStrs |> List.filterMap String.toInt
            in
            String.fromInt <| Set.size <| Set.fromList nums

        _ ->
            "fail"


solveH : List String -> String
solveH inputs =
    case Maybe.map (String.split " " >> List.filterMap String.toInt) <| List.head inputs of
        Just (n :: y :: []) ->
            let
                candidateAnswer =
                    List.head <|
                        (List.range 0 n
                            |> List.concatMap
                                (\manN ->
                                    List.range 0 (n - manN)
                                        |> List.filterMap
                                            (\gosenN ->
                                                if y == manN * 10000 + gosenN * 5000 + (n - manN - gosenN) * 1000 then
                                                    Just <|
                                                        String.fromInt manN
                                                            ++ " "
                                                            ++ String.fromInt gosenN
                                                            ++ " "
                                                            ++ String.fromInt (n - manN - gosenN)

                                                else
                                                    Nothing
                                            )
                                )
                        )
            in
            Maybe.withDefault "-1 -1 -1" candidateAnswer

        _ ->
            "fail"


solveI : List String -> String
solveI inputs =
    let
        divide =
            [ "dream", "dreamer", "erase", "eraser" ] |> List.map String.reverse

        loop : String -> Bool
        loop str =
            let
                dd =
                    divide
                        |> List.filterMap
                            (\d ->
                                let
                                    subStr =
                                        String.slice 0 (String.length d) str
                                in
                                if subStr == d then
                                    Just d

                                else
                                    Nothing
                            )
            in
            case dd of
                d :: [] ->
                    let
                        rest =
                            (String.slice (String.length d) <| String.length str) str
                    in
                    if String.length rest == 0 then
                        True

                    else
                        loop rest

                _ ->
                    False
    in
    case List.head inputs of
        Just s ->
            if loop <| String.reverse s then
                "YES"

            else
                "NO"

        Nothing ->
            "fail"


main : Program (List String) Int msg
main =
    Platform.worker
        -- ここをsolveAなど切り替える
        { init = \inputs -> ( 0, submit <| solveI inputs )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
