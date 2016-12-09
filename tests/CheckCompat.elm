module CheckCompat exposing (..)

import Check
import Test

that =
    flip Check.that


is =
    flip Check.is


for =
    flip Check.for


true =
    flip Check.true

test str expr =
    Test.test str <| \_ -> expr
