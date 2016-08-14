module Main exposing (..)

import ElmTest as Test
import Tests


main : Program Never
main =
    Test.runSuite Tests.all
