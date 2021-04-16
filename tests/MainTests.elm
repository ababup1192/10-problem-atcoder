module MainTests exposing (tests)

import Expect
import Main exposing (solveA, solveB, solveC, solveD, solveE, solveF)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Test"
        [ describe "solveA"
            [ test "case1" <|
                \_ ->
                    [ "3 4" ]
                        |> solveA
                        |> Expect.equal "Even"
            , test "case2" <|
                \_ ->
                    [ "1 21" ]
                        |> solveA
                        |> Expect.equal "Odd"
            ]
        , describe "solveB"
            [ test "case1" <|
                \_ ->
                    [ "101" ]
                        |> solveB
                        |> Expect.equal "2"
            , test "case2" <|
                \_ ->
                    [ "000" ]
                        |> solveB
                        |> Expect.equal "0"
            ]
        , describe "solveC"
            [ test "case1" <|
                \_ ->
                    [ "3", "8 12 40" ]
                        |> solveC
                        |> Expect.equal "2"
            , test "case2" <|
                \_ ->
                    [ "4", "5 6 8 10" ]
                        |> solveC
                        |> Expect.equal "0"
            , test "case3" <|
                \_ ->
                    [ "6", "382253568 723152896 37802240 379425024 404894720 471526144" ]
                        |> solveC
                        |> Expect.equal "8"
            ]
        , describe "solveD"
            [ test "case1" <|
                \_ ->
                    [ "2", "2", "2", "100" ]
                        |> solveD
                        |> Expect.equal "2"
            , test "case2" <|
                \_ ->
                    [ "5", "1", "0", "150" ]
                        |> solveD
                        |> Expect.equal "0"
            , test "case3" <|
                \_ ->
                    [ "30", "40", "50", "6000" ]
                        |> solveD
                        |> Expect.equal "213"
            ]
        , describe "solveE"
            [ test "case1" <|
                \_ ->
                    [ "20 2 5" ]
                        |> solveE
                        |> Expect.equal "84"
            , test "case2" <|
                \_ ->
                    [ "10 1 2" ]
                        |> solveE
                        |> Expect.equal "13"
            , test "case3" <|
                \_ ->
                    [ "100 4 16" ]
                        |> solveE
                        |> Expect.equal "4554"
            ]
        , describe "solveF"
            [ test "case1" <|
                \_ ->
                    [ "2", "3 1" ]
                        |> solveF
                        |> Expect.equal "2"
            , test "case2" <|
                \_ ->
                    [ "3", "2 7 4" ]
                        |> solveF
                        |> Expect.equal "5"
            , test "case3" <|
                \_ ->
                    [ "4", "20 18 2 18" ]
                        |> solveF
                        |> Expect.equal "18"
            ]
        ]
