port module Main exposing (..)

import Tests exposing (claims, traditionalTest)
import Check exposing (quickCheck)
import Check.Test exposing (evidenceToTest)
import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit <| Test.describe "A Test Suite" [ evidenceToTest (quickCheck claims), traditionalTest ]



port emit : ( String, Value ) -> Cmd msg
