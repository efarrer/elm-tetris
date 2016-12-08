module TetrisTests exposing (..)

import Test exposing (..)
import Expect
import String
import Tetris
import Debug exposing (..)
import Color exposing (..)


all : Test
all =
    describe "Tetris Test Suite"
        [ test "rowFilled not filled at start" <|
            \() ->
                let
                    ( model, msg ) =
                        Tetris.init
                in
                    Expect.equal False <| Tetris.rowFilled 0 model.board
        , test "rowFilled detect filled row" <|
            \() ->
                let
                    y =
                        (Tetris.height - 1)

                    ( model, msg ) =
                        Tetris.init
                in
                    Expect.equal True
                        (model.board
                            |> Tetris.setBoardColors (List.map (\x -> ( x, y, Color.red )) <| List.range 0 Tetris.width)
                            |> Tetris.rowFilled y
                        )
        ]
