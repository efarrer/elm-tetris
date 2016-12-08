module Main exposing (..)

import Html exposing (program)
import Tetris


main =
    program
        { init = Tetris.init
        , view = Tetris.view
        , update = Tetris.update
        , subscriptions = Tetris.subscriptions
        }
