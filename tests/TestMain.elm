module TestMain exposing (aliveNeighborsTest, getCellStatusBasedOnAliveNeighborsTest, getCellTest, isMaybeAliveTest, neighborIndexesTest, neighborsTest, normalizeSizeTest, previousStepTest, toggleAutoplayTest, toggleCellStatusTest, toggleCellTest)

import Array
import Expect exposing (..)
import Main exposing (Autoplay(..), Cell(..))
import Test exposing (Test, describe, test)


normalizeSizeTest : Test
normalizeSizeTest =
    describe "normalizeSize"
        [ test "returns 8 for non integer string" <|
            \_ ->
                Main.normalizeSize "some string"
                    |> Expect.equal 8
        , test "returns 8 for inputs smaller than 8" <|
            \_ ->
                Main.normalizeSize "6"
                    |> Expect.equal 8
        , test "returns 96 for inputs greater than 96" <|
            \_ ->
                Main.normalizeSize "100"
                    |> Expect.equal 96
        , test "returns size as int for valid inputs" <|
            \_ ->
                Main.normalizeSize "56"
                    |> Expect.equal 56
        ]


neighborIndexesTest : Test
neighborIndexesTest =
    describe "neighborIndexes"
        [ test "returns correct neighbor indexes for given index" <|
            \_ ->
                Main.neighborIndexes 8 9
                    |> Expect.equal
                        [ ( True, 0 )
                        , ( True, 1 )
                        , ( True, 2 )
                        , ( True, 8 )
                        , ( True, 10 )
                        , ( True, 16 )
                        , ( True, 17 )
                        , ( True, 18 )
                        ]
        , test "returns correct neighbor indexes for given index on left border" <|
            \_ ->
                Main.neighborIndexes 8 8
                    |> Expect.equal
                        [ ( False, -1 )
                        , ( True, 0 )
                        , ( True, 1 )
                        , ( False, 7 )
                        , ( True, 9 )
                        , ( False, 15 )
                        , ( True, 16 )
                        , ( True, 17 )
                        ]
        , test "returns correct neighbor indexes for given index on right border" <|
            \_ ->
                Main.neighborIndexes 8 15
                    |> Expect.equal
                        [ ( True, 6 )
                        , ( True, 7 )
                        , ( False, 8 )
                        , ( True, 14 )
                        , ( False, 16 )
                        , ( True, 22 )
                        , ( True, 23 )
                        , ( False, 24 )
                        ]
        , test "returns correct neighbor indexes for given index on top border" <|
            \_ ->
                Main.neighborIndexes 8 4
                    |> Expect.equal
                        [ ( True, -5 )
                        , ( True, -4 )
                        , ( True, -3 )
                        , ( True, 3 )
                        , ( True, 5 )
                        , ( True, 11 )
                        , ( True, 12 )
                        , ( True, 13 )
                        ]
        , test "returns correct neighbor indexes for given index on bottom border" <|
            \_ ->
                Main.neighborIndexes 8 61
                    |> Expect.equal
                        [ ( True, 52 )
                        , ( True, 53 )
                        , ( True, 54 )
                        , ( True, 60 )
                        , ( True, 62 )
                        , ( True, 68 )
                        , ( True, 69 )
                        , ( True, 70 )
                        ]
        ]


neighborsTest : Test
neighborsTest =
    describe "neighbors"
        [ test "returns correct list of neighbor indexes for given index" <|
            \_ ->
                Main.neighbors 8 9
                    |> Expect.equal [ 0, 1, 2, 8, 10, 16, 17, 18 ]
        , test "returns correct list of neighbor indexes for given index on left border" <|
            \_ ->
                Main.neighbors 8 8
                    |> Expect.equal [ 0, 1, 9, 16, 17 ]
        , test "returns correct list of neighbor indexes for given index on right border" <|
            \_ ->
                Main.neighbors 8 15
                    |> Expect.equal [ 6, 7, 14, 22, 23 ]
        , test "returns correct list of neighbor indexes for given index on top border" <|
            \_ ->
                Main.neighbors 8 4
                    |> Expect.equal [ -5, -4, -3, 3, 5, 11, 12, 13 ]
        , test "returns correct list of neighbor indexes for given index on bottom border" <|
            \_ ->
                Main.neighbors 8 61
                    |> Expect.equal [ 52, 53, 54, 60, 62, 68, 69, 70 ]
        ]


aliveNeighborsTest : Test
aliveNeighborsTest =
    let
        board =
            Array.fromList <|
                List.concat
                    [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    ]
    in
    describe "aliveNeighbors"
        [ test "returns correct number of alive neighbors for cell with 8 alive neighbors" <|
            \_ ->
                Main.aliveNeighbors board 8 9
                    |> Expect.equal 8
        , test "returns correct number of alive neighbors for cell with 2 alive neighbors" <|
            \_ ->
                Main.aliveNeighbors board 8 3
                    |> Expect.equal 2
        , test "returns correct number of alive neighbors for cell with 3 alive neighbors" <|
            \_ ->
                Main.aliveNeighbors board 8 11
                    |> Expect.equal 3
        , test "returns correct number of alive neighbors for cell with 0 alive neighbors" <|
            \_ ->
                Main.aliveNeighbors board 8 34
                    |> Expect.equal 0
        ]


toggleCellStatusTest : Test
toggleCellStatusTest =
    describe "toggleCellStatus"
        [ test "returns Dead if passed in cell is Alive" <|
            \_ ->
                Main.toggleCellStatus Alive
                    |> Expect.equal Dead
        , test "returns Alive if passed in cell is Dead" <|
            \_ ->
                Main.toggleCellStatus Dead
                    |> Expect.equal Alive
        ]


getCellTest : Test
getCellTest =
    let
        board =
            Array.fromList <|
                List.concat
                    [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    ]
    in
    describe "getCell"
        [ test "returns correct cell status for alive cell" <|
            \_ ->
                Main.getCell 1 board
                    |> Expect.equal Alive
        , test "returns correct cell status for dead cell" <|
            \_ ->
                Main.getCell 12 board
                    |> Expect.equal Dead
        , test "returns dead status for index out of bounds" <|
            \_ ->
                Main.getCell 87 board
                    |> Expect.equal Dead
        ]


toggleCellTest : Test
toggleCellTest =
    let
        board =
            Array.fromList <|
                List.concat
                    [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    ]
    in
    describe "toggleCell"
        [ test "returns board with toggled cell at index 10 from Alive to Dead" <|
            \_ ->
                Main.toggleCell 10 board
                    |> Expect.equal
                        (Array.fromList <|
                            List.concat
                                [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                ]
                        )
        , test "returns board with toggled cell at index 12 from Dead to Alive" <|
            \_ ->
                Main.toggleCell 12 board
                    |> Expect.equal
                        (Array.fromList <|
                            List.concat
                                [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Alive, Dead, Alive, Dead, Alive, Dead, Dead, Dead ]
                                , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                ]
                        )
        , test "returns original board if index is out of bounds" <|
            \_ ->
                Main.toggleCell 89 board
                    |> Expect.equal
                        (Array.fromList <|
                            List.concat
                                [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                                ]
                        )
        ]


isMaybeAliveTest : Test
isMaybeAliveTest =
    describe "isMaybeAlive"
        [ test "returns true if cell is alive" <|
            \_ ->
                Main.isMaybeAlive (Just Alive)
                    |> Expect.equal True
        , test "returns false if cell is dead" <|
            \_ ->
                Main.isMaybeAlive (Just Dead)
                    |> Expect.equal False
        , test "returns false for Nothing" <|
            \_ ->
                Main.isMaybeAlive Nothing
                    |> Expect.equal False
        ]


getCellStatusBasedOnAliveNeighborsTest : Test
getCellStatusBasedOnAliveNeighborsTest =
    let
        board =
            Array.fromList <|
                List.concat
                    [ [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Alive, Alive, Dead, Dead, Dead, Dead, Dead ]
                    , [ Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    , [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
                    ]
    in
    describe "getCellStatusBasedOnAliveNeighbors"
        [ test "returns Alive for Alive cell with 2 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 0 Alive
                    |> Expect.equal Alive
        , test "returns Dead for Dead cell with 2 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 3 Dead
                    |> Expect.equal Dead
        , test "returns Alive for Alive cell with 3 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 16 Alive
                    |> Expect.equal Alive
        , test "returns Alive for Dead cell with 3 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 11 Dead
                    |> Expect.equal Alive
        , test "returns Dead for Dead cell with more than 3 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 9 Dead
                    |> Expect.equal Dead
        , test "returns Dead for Alive cell with more than 3 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 17 Alive
                    |> Expect.equal Dead
        , test "returns Dead for cell with 1 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 27 Dead
                    |> Expect.equal Dead
        , test "returns Dead for cell with 0 Alive neighbors" <|
            \_ ->
                Main.getCellStatusBasedOnAliveNeighbors board 8 36 Dead
                    |> Expect.equal Dead
        ]


toggleAutoplayTest : Test
toggleAutoplayTest =
    describe "toggleAutoplay"
        [ test "returns Paused if was Playing" <|
            \_ ->
                Main.toggleAutoplay Playing
                    |> Expect.equal Paused
        , test "returns Playing if was Paused" <|
            \_ ->
                Main.toggleAutoplay Paused
                    |> Expect.equal Playing
        ]


previousStepTest : Test
previousStepTest =
    let
        board =
            Array.repeat 64 Alive

        history =
            [ Array.repeat 64 Dead, Array.repeat 64 Alive ]
    in
    describe "previousStep"
        [ test "returns first board from history for history with entries" <|
            \_ ->
                Main.previousStep history board
                    |> Expect.equal (Array.repeat 64 Dead)
        , test "returns original board when history is empty" <|
            \_ ->
                Main.previousStep [] board
                    |> Expect.equal (Array.repeat 64 Alive)
        ]
