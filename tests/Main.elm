port module Main exposing (..)

import TetrisTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit TetrisTests.all


port emit : ( String, Value ) -> Cmd msg
