module MainTests exposing (tests)

import Expect
import Main exposing (solveA, solveB)
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
        ]
